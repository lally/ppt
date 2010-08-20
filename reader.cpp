#include "metric_types.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/shm.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <algorithm>
FILE *dest;

const int MIN_DELAY_MS = 100;
const int MAX_DELAY_MS = 2000;

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
	metrics_beta1_seqno_t last_seqno;

	last_seqno=0;

	delay = 1000;
	if (args < 4) {
		printf ("usage: %s <shmid> <size> output\n", argv[0]);
		printf ("where:\n");
		printf ("   shmid is a shared memory handle\n");
		printf ("   size is the capacity, in frames, of the "
				"shared memory segment\n");
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

	while (1) {
		unsigned int seqno;
		unsigned int count;
		count =0;

		// save the current minimum sequence number for this scan.
		unsigned cur_min_seqno = cur->seqno;

		// Only bother reading if there's anything new.
		if (cur_min_seqno > last_seqno) {
			do {
				fprintf(dest, "%10.5f\n", cur->beta1);
				seqno = cur++->seqno;
				count++;
				if (cur >= end) {
					cur = start;
				}
			} while (cur->seqno > cur_min_seqno);
		}
	
		fprintf(stderr,
				"read-end: sequences %d to %d, count=%d, "
				"delay=%d, read%%=%4.1f\n",
				last_seqno, seqno, count, delay, (100.0 * count) / size);

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

		bool alter = false;
		if (count > (size - size/8)) {
			// adjust the delay down
			alter = true;
		}
		else if (count < (size/8)) {
			alter = true;
		}

		if (alter) {
			// In 'delay' milliseconds, we read 'count' values.  Set the new
			// delay to use this measured read-rate to sleep until half the
			// buffer is full.  The 'rate' below is the appearance rate of
			// new data, per millisecond.
			double rate = (0.0 + count) /delay;
			double new_delay = size * 0.5 / rate;
			fprintf(stderr, 
					"[rate=%6f val/ms, bufsz=%d] Shifting read rate from %d to %6.2f\n",
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
