#include <sys/types.h>
#include <sys/shm.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
int main(int args, char **argv) {
	unsigned long n;
	sscanf(argv[1], "%d", &n);
	printf("Starting with %s\n", argv[1]);
	int fd = shmget(n, 16384, (SHM_R | SHM_W));
	void *pa = (void*)shmat(n, 0, 0);
	char *start, *end;
    start = (char*) pa;
    end = start + 16384;
	int generation = -1;
	char last = 0xFF;
	char *s;
	while (1) {
		s=start;
		printf("reading chars\n");
		fflush(stdout);
		while (s<end) {
			if (*s != last) {
				generation++;
				last = *s;
			}
			s++;
		}
		printf("r%02x %d\n", last, generation);
		sleep(1);
	}            

	return 0;
	
}
