
#include "beta2.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <signal.h>
#include <time.h>
#include <limits.h>
#include <assert.h>

int mid;
void *key_mem;

int TEST_WRITER_VERSION=1024;
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
    //    metrics_beta1_seqno_t max_val = INT_MAX;
    int size;
    int key;

    //
    // Step 1: setup the shared memory, print out its parameters, and
    // and just start writing.
    //

    // printf ("sizeof pptframe_beta2_t: %d bytes\n",
    //         sizeof (pptframe_beta2_t));

    /*    if (args < 2) {
          printf("usage: %s <size> [count] \n", argv[0]);
          puts  ("      size: number of frames in the buffer");
          puts  ("      count: if present, how many times to fill the array before stopping");
          exit(1);
          } */

    size = 10; //atoi(argv[1]);
    int iter_max = -1;
    if (args > 2) {
        iter_max= atoi(argv[2]);
    }

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
#define SHM_R 00400
#define SHM_W 00200
#endif  
  
    /*    mid = shmget(key, size * sizeof (pptframe_beta2_t), 
          IPC_CREAT | IPC_EXCL | SHM_R | SHM_W);
          if (mid == -1) {
          perror("shmget");
          exit(1);
          }

          printf ("handle = %d, size = %d elements, %d bytes\n",
          mid, size, size * sizeof (pptframe_beta2_t));
          signal(SIGINT, kill);
          signal(SIGKILL, kill);


    */
    double whole_interval = 1000.0 + (1000.0 * new_freq());
    double interval = whole_interval / 31.25;

    timeval last_upd, now;
    gettimeofday(&last_upd, 0);

    double value = 0.0;
    int counter = 0;

    // a little bit of a hack.
    //    _ppt_hmem_beta2 = mid;

    int iter_count = 0;
    while (iter_count != iter_max) {
        // when wrapping seqno, skip zero.
        if (counter & 1) {
            WRITE_FRAMECOUNT_COUNT(counter);
            WRITE_FRAMECOUNT_XI(counter * 2);
            timeval t;
            gettimeofday(&t, 0);
            WRITE_FRAMECOUNT_START(t);
            t.tv_sec += 1;
            WRITE_FRAMECOUNT_END(t);
            WRITE_FRAMECOUNT_COUNT2(counter+1);
            ppt_write_framecount_frame();
        } else {
            WRITE_XS_XI(counter);
            ppt_write_xs_frame();
        }
        counter++;
        usleep((useconds_t) (1000.0 * interval));
        gettimeofday(&now, 0);

        assert(_ppt_frame_framecount.ppt_type == 1);
        assert(_ppt_frame_xs.ppt_type == 2);
        if (iter_count++ < 0) {
            iter_count = 0;
        }
   
        if (now.tv_sec > last_upd.tv_sec) {
            do {
                whole_interval = 1000.0 + (1000.0 * new_freq());
            } while (whole_interval < 50.0 
                     || whole_interval > 15000.0);

            interval = whole_interval / 31.25;
            last_upd = now;
            printf("%d/%d: %d Hz, whole_interval=%fms, interval=%fms, "
                   "seqno=%d\n",  mid, size, (int) interval,
                   whole_interval, interval, counter);
        }
    }

}
