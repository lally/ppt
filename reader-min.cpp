#include "metric_types.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/shm.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
FILE *dest;

const int MIN_DELAY_MS = 100;
const int MAX_DELAY_MS = 2000;

unsigned long min(unsigned long a, unsigned long b) {
    return a<b?a:b;
}

#define BUFFER_SIZE 65536

int main(int args, char **argv) {
    int shmid, size;
    int  delay; // delay per loop iteration.  altered as we go.  In
                // millisecs. 
    metrics_beta1_frame *start, *cur, *end;
    metrics_beta1_seqno_t last_seqno;

	
    last_seqno=0;

    delay = 1000;
    if (args < 3) {
        printf ("usage: %s <shmid> output\n", argv[0]);
        printf ("where:\n");
        printf ("   shmid is a shared memory handle\n");
        printf ("   output is the destination filename\n");
        exit(1);
    }

    shmid = atoi(argv[1]);

    if (shmid <= 0) {
        puts("invalid shmid");
        exit(1);
    }

    struct shmid_ds shm_inf;
    if (0 != shmctl(shmid, IPC_STAT, &shm_inf)) {
        perror(argv[1]);
        exit(1);
    }

    size = shm_inf.shm_segsz / sizeof (metrics_beta1_frame);

    if (0 == (dest = fopen(argv[2], "w+"))) {
        perror(argv[2]);
        exit(1);
    }

    start = (metrics_beta1_frame *) shmat(shmid, 0, 0);
    if (start == (metrics_beta1_frame*) -1) {
        perror("shmat");
        exit(1);
    }

    end = start + size;
    cur = start;

    while (dest) {
        // 'seqno' is the last seen sequence number in this scan.
        // 'last_seqno' is the last consumed sequence number of hte
        // prior scan.  The difference only comes up when we're trying
        // to detect a noncontiguously-numbered frame.
        metrics_beta1_seqno_t seqno;
        unsigned int count;
        count = 0;
        seqno = last_seqno;

        while (cur->seqno > seqno 
               || (cur->seqno <= min(last_seqno - size, 0))
               || (count == 0 && cur->seqno != last_seqno)) {
            printf (".");
            if (dest) {
                fprintf(dest, "%10.5f", cur->beta1);
            }
            seqno = cur->seqno;
            count++;
            if (++cur >= end) {
                cur = start;
            }
		
        }

        // update 'last_seqno' to be the last frame we read.
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

        bool recalc_delay = false;
		
        // Check if we've hit the high or low water marks.
        if (count > (size - size/8) || count < (size/8)) {
            // Recalculate the per-loop delay.
            recalc_delay = true;
        }

        if (!count) {
            delay = MAX_DELAY_MS;
        } else if (recalc_delay) {
            // In 'delay' milliseconds, we read 'count' values.  Use
            // this as an estimator for the current write rate. Set
            // the new delay to use this measured write-rate to sleep
            // until half the buffer is full.  The 'rate' below is the
            // appearance rate of new data, per millisecond.
            double rate = (0.0 + count) /delay;
            double new_delay = size * 0.5 / rate;
            delay = (int) new_delay;
        }

        if (delay < MIN_DELAY_MS) {
            // hard-coded 100ms floor
            delay = MIN_DELAY_MS;
        }
        else if (delay > MAX_DELAY_MS) {
            // hard-coded 2 sec ceiling
            delay = MAX_DELAY_MS;
        }

        usleep(delay * 1000);

    }
}
