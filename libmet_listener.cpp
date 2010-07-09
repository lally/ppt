#include <sys/types.h>
#include <sys/shm.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

struct Table {
	unsigned int frame_index;
	unsigned int frame_buffer_size;
	double       data[1];
};

int main(int args, char **argv) {
	unsigned long mid, size;
	if (args < 3) {
		return 1;
	}
	sscanf(argv[1], "%d", &mid);
	sscanf(argv[2], "%d", &size);
	
	printf("Starting with %s %s\n", argv[1], argv[2]);

	int outfd = open("beta1.tbl", O_CREAT | O_EXCL, 0600);
	if (outfd < 0) {
		perror("open(beta1.tbl)");
		return 1;
	}
	FILE *out = fdopen(outfd, "w+");
	if (!out) {
		perror("fdopen(beta1.tbl)");
		return 1;
	}
	int fd = shmget(mid, size, SHM_R);
	void *pa = (void*)shmat(mid, 0, 0);
	Table *table;
    table = (Table*) pa;
	struct timespec tp;
	tp.tv_sec = 0;
	tp.tv_nsec = 500000000;
	int sequential_failure_count = 0;

	fprintf(out, "beta_one\n");
	
	/*
	 * Algorithm: read halves of the buffer in blocks, idling while it
	 * fills. If we pass 3 loop iterations (with the delays in the
	 * middle) and get zero data, stop.
	 */
	const int sz = table->frame_buffer_size;
	const int threshold = sz/2;
	int last_index = -1;
	enum { LOWER, UPPER} previously_done = UPPER;
	while (sequential_failure_count < 3) {
		//
		// We won't read data every time.  We're just waiting for
		//   halves of the buffer to fill, and then reading the entire
		//   half at once.
		if (table->frame_index < threshold
			&& previously_done != LOWER) {
			// catch up, copy from the back
			for (int i=0; i<threshold; ++i) {
				fprintf(out, "%10.5f\n", table->data[i]);
			}
			fflush(out);
			previously_done = LOWER;
			sequential_failure_count = 0;
		}
		else if (table->frame_index > threshold
				 && previously_done != UPPER) {
			for (int i=threshold; i < sz; ++i) {
				fprintf(out, "%10.5f\n", table->data[i]);
			}
			fflush(out);
			previously_done = UPPER;
			sequential_failure_count = 0;
		}
		else if (last_index == table->frame_index) {
			sequential_failure_count++;
		}
		/* else it's just slow progress */

		
		last_index = table->frame_index;
		nanosleep(&tp, 0);
	}
	fflush(out);
	fclose(out);
	return 0;
}
