#include "metric_types.h"
#include <sys/types.h>
#include <sys/shm.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
FILE *dest;

void close_handler(int sig) {
	if (dest) {
		fflush(dest);
		if (sig != SIGUSR1) {
			fclose(dest);
			dest = 0;
		}
	}
}

#define BUFFER_SIZE 65536

int main(int args, char **argv) {
	int shmid, size;
	char *buf;
	int  delay; // delay per loop iteration.  altered as we go.  In
				// millisecs. 
	metrics_beta1_frame *start, *cur, *end;
	metrics_beta1_seqno last_seqno;

	last_seqno=0;

	delay = 1000;
	if (args < 4) {
		printf ("usage: %s <shmid> <size> output\n", argv[0]);
		printf ("where:\n");
		printf ("   shmid is a shared memory handle\n");
		printf ("   size is the capacity, in frames, of the shared memory segment\n");
		printf ("   output is the destination filename\n");
		exit(1);
	}

	shmid = atoi(argv[1]);
	size = atoi(argv[2]);

	if (shmid <= 0 || size <= 1) {
		puts("invalid shmid or size!");
		exit(1);
	}

	if (0 == (dest = fopen(argv[3], "w+"))) {
		perror(argv[3]);
		exit(1);
	}

	signal(SIGINT, close_handler);
	signal(SIGHUP, close_handler);
	signal(SIGUSR1, close_handler);
	
	start = (metrics_beta1_frame *) shmat(shmid, 0, 0);
	if (!start) {
		perror("shmat");
		exit(1);
	}
	end = start + size;
	cur = start;

	buf = (char*) malloc(BUFFER_SIZE);
	setvbuf(dest, buf, _IOFBF, BUFFER_SIZE);
	fprintf (dest, "beta1\n");

	while (1) {
		unsigned int seqno;
		unsigned int count;
		count =0;
		while (cur->seqno >= last_seqno) {
			fprintf("%10.5f\n", cur->beta1);
			seqno = cur++->seqno;
			count++;
		}
		last_seqno = seqno;

		//
		// AUTO TIMING ADJUSTMENT
		//
		// The low-water mark, when we're reading too little of the
		// buffer per iteration, is size/8.  The high water mark is
		// size - size/8.  If we hit the low water mark, we'll
		// increase the delay, and decrease it if we're hitting 

		// The current delay is linearly related to the write rate
		// on the other side, meaning that the proportion of 'size'
		// that 'count' is, indicates the percent of the
		// time-until-buffer-full the delay is.
		//
		// When we hit either boundary, we'll reset the delay to hit
		// the 50%-fill point in the buffer.

		double rate_prop = (0.0 + count) / (0.0 + size);
		bool alter = false;
		if (count > (size - size/8)) {
			// adjust the delay down
			alter = true;
		}
		else if (count < (size/8)) {
			alter = true;
		}

		if (alter) {
			int new_delay = delay * rate_prop * 0.5;
			printf(stderr, "Shifting read rate from %i to %i\n",
				   delay, new_delay);
			delay= new_delay;
		}
		if (delay < 100) {
			// hard-coded 100ms floor
			delay = 100;
			printf(stderr, "-- Delay floor hit, setting to 100ms\n");
		}
		else if (delay > 2000) {
			// hard-coded 2 sec ceiling
			printf(stderr, "-- Delay ceiling hit, setting to 2 seconds\n");
		}
		sleep(delay * 1000);

	}
}
