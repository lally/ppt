#include "metric_types.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/shm.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <algorithm>
#include <vector> 
#include <assert.h>
FILE *dest;

const int MIN_DELAY_MS = 100;
const int MAX_DELAY_MS = 2000;

unsigned long min(unsigned long a, unsigned long b) {
    return a<b?a:b;
}

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
    metrics_beta1_seqno_t last_seqno, max_seqno;

    max_seqno = metrics_beta1_seqno_max;
    const char * e_max;
    if ((e_max = getenv("MAX_SEQNO"))) {
	max_seqno = (metrics_beta1_seqno_t) atof(e_max);
    }
    printf ("max_seqno=%f\n", (double) max_seqno);
	
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

    if (shm_inf.shm_segsz % sizeof (metrics_beta1_frame)) {
	puts("memory segment size is a little off!");
	exit(1);
    }
    size = shm_inf.shm_segsz / sizeof (metrics_beta1_frame);

    if (0 == (dest = fopen(argv[2], "w+"))) {
	perror(argv[2]);
	exit(1);
    }

    signal(SIGINT, close_handler);
    signal(SIGHUP, close_handler);
    signal(SIGUSR1, close_handler);
	
    start = (metrics_beta1_frame *) shmat(shmid, 0, 0);
    if (start == (metrics_beta1_frame*) -1) {
	perror("shmat");
	exit(1);
    }
    end = start + size;
    cur = start;

    fprintf(stderr, "Loaded shmid=%d, start=0x%08x, end=0x%08x\n",
	    shmid, start, end);

    buf = (char*) malloc(BUFFER_SIZE);
    setvbuf(dest, buf, _IOFBF, BUFFER_SIZE);
    fprintf (dest, "beta1\n");

    std::vector<metrics_beta1_seqno_t> seqnos;
    seqnos.resize(size);

    while (dest) {
	metrics_beta1_seqno_t seqno, start_seqno;
	double b1_first, b1_last;
	bool first = true;
	unsigned int count;
	count =0;

	seqno = last_seqno;
	start_seqno = cur->seqno;

	int inc_cnt = 0;
	int zero_cnt = 0;
	assert(last_seqno >= 0);

	// snapshot the buffer
	for(int i=0; i<size; ++i) {
	    seqnos[i] = start[i].seqno;
	    if (start + i != cur) {
		printf("%4d  ", seqnos[i]);
	    }
	    else {
		printf("%4d* ", seqnos[i]);
	    }
	}
	printf ("; cur->seqno=%4d, seqno=%4d, last_seqno=%4d\n", cur->seqno, seqno, last_seqno);
	
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
	printf("\n");

	fprintf(stderr,
		"Seq: %d(%f) to %d(%f), inc=%d, zer=%d, count=%d, "
		"delay=%d, read%%=%4.1f\n",
		last_seqno, b1_first, seqno-1, b1_last, 
		inc_cnt, zero_cnt,
		count, delay, 
		(100.0 * count) / size);

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
	    fprintf(stderr, "No values in last %d ms.  Minimizing read rate\n",
		    delay);
	    delay = MAX_DELAY_MS;
	} else if (recalc_delay) {
	    // In 'delay' milliseconds, we read 'count' values.  Use
	    // this as an estimator for the current write rate. Set
	    // the new delay to use this measured write-rate to sleep
	    // until half the buffer is full.  The 'rate' below is the
	    // appearance rate of new data, per millisecond.
	    double rate = (0.0 + count) /delay;
	    double new_delay = size * 0.5 / rate;
	    fprintf(stderr, 
		    "[rate=%6f val/ms, bufsz=%d] Shifting read rate from "
		    "%d to %6.2f\n",
		    rate, size, delay, new_delay);
	    delay = (int) new_delay;
	}

	if (delay < MIN_DELAY_MS) {
	    // hard-coded 100ms floor
	    delay = MIN_DELAY_MS;
	    fprintf(stderr, "   -- Delay floor hit, setting to %dms\n",
		    MIN_DELAY_MS);
	}
	else if (delay > MAX_DELAY_MS) {
	    // hard-coded 2 sec ceiling
	    fprintf(stderr, 
		    "   -- Delay ceiling hit, setting to %3.2f seconds\n",
		    MAX_DELAY_MS / 1000.0);
	    delay = MAX_DELAY_MS;
	}

	usleep(delay * 1000);

    }
}
