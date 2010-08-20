#include "metric_types.h"


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <signal.h>
#include <time.h>

int mid;
void *key_mem;

/*
  http://en.wikipedia.org/wiki/Box-Muller_transform
*/
double uniform_rand() {
	double v = random();
	return v/0x7fffffff;
}

double new_freq () {
	double u1 = uniform_rand();
	double u2 = uniform_rand();
  
	double R = sqrt( -2.0 * log(u1));
  
	return R*cos(u2 * 2 * M_PI);
}


void kill(int sig) {
	printf("signal %d: Trying to die on shm id %d\n", sig, mid);
	shmdt(key_mem);
	/*  struct shmid_ds st;
		do {
		sleep(1);
		shmctl(mid, IPC_STAT, &st);
		} while (st.shm_nattch > 0); */
	int ret = shmctl(mid, IPC_RMID, 0);
	if (ret) {
		perror("shmctl(IPC_RMID)");
	}
	exit(0);
}

/*
 * Test Writer
 * -----------

 * Intended to simulate the setup & injection of the variable.

 */
int main(int args, char **argv) {

	int size;
	int key;

	//
	// Step 1: setup the shared memory, print out its parameters, and
	// and just start writing.
	//

	if (args < 2) {
		printf("usage: %s <size>\n", argv[0]);
		puts  ("      size: number of frames in the buffer");
		exit(1);
	}

	size = atoi(argv[1]);

	if (size < 1) {
		puts("size must be at least one.");
		exit(1);
	}

	key = ftok(argv[0], size);
	if (key == -1) {
		perror(argv[0]);
		exit(1);
	}

#ifdef sun
#define IPC_R 00400
#define IPC_W 00200
#endif  
  
	mid = shmget(key, size * sizeof (metrics_beta1_frame), 
				 IPC_CREAT | IPC_EXCL | IPC_R | IPC_W);
	if (mid == -1) {
		perror("shmget");
		exit(1);
	}

	metrics_beta1_frame *start, *cur, *end;
	key_mem = shmat(mid, 0,0);
	start = (metrics_beta1_frame *) key_mem;
	printf ("handle = %d, size = %d elements, %d bytes\n",
			mid, size, size * sizeof (metrics_beta1_frame));
	if (start == (metrics_beta1_frame *) -1) {
		perror("shmat");
		exit(1);
	}
	end = start + size;
	cur = start;

	signal(SIGINT, kill);
	signal(SIGKILL, kill);

	/*

	  Question: how frequently to I alter the I/O rate?  How high is the
	  jitter?  Per-output jitter is too high.  This will accumulate too
	  quickly.
    
	  Sample at ceil(1 Hz).  Choose the per-write interval from
	  standard-normal to 31.25hz.  Simple method: Scale to 1000ms, then
	  div 32.
	*/


	double whole_interval = 1000.0 + (1000.0 * new_freq());
	double interval = whole_interval / 31.25;

	timeval last_upd, now;
	gettimeofday(&last_upd, 0);

	metrics_beta1_seqno_t seqno = 0;
	double value = 0.0;
	int counter = 0;
	while (1) {
		cur->seqno = seqno++;
		cur->beta1 = value;
		counter++;
		value += 1.0;
		if (++cur >= end) {
			cur = start;
		}

		usleep((useconds_t) (1000.0 * interval));
		gettimeofday(&now, 0);

		if (now.tv_sec > last_upd.tv_sec) {
			do {
				whole_interval = 1000.0 + (1000.0 * new_freq());
			} while (whole_interval < 50.0 || whole_interval > 15000.0);
			interval = whole_interval / 31.25;
			last_upd = now;
			printf("%d/%d: %d Hz, whole_interval=%fms, interval=%fms, "
				   "seqno=%d\n",  mid, size,
				   counter, whole_interval, interval, seqno);
			counter = 0;
		}
	}

}
