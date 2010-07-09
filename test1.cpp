#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/shm.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

	int mid;
	mid = shmget(IPC_PRIVATE, 16384, (SHM_R | SHM_W));
	if (mid < 0) {
		perror ("1: shmget");
		return 0;
	}
	
    void *pa = shmat(mid, 0, 0);
    if (!pa || (((int) pa) == -1)) {
		perror("1: shmat");
		return 1;
    }

    char *start, *end;
    start = (char*) pa;
    end = start + 16384;

    pid_t pid;
    if (pid = fork()) {
        char c=0;
        char *s;
		// we're the writer.
        while (1) {
			s=start;
            while (s<end) {
				*s++ = c;
            }
            c++;
            printf("w%02x\n", c);
            sleep(1);
        }
    }
    else {
		char buf[512];
		sprintf(buf, "%d", mid);
		char *args[] = {
			"test2",
			buf,
			0
		};
		printf("exec'ing with arg=%s\n", buf);
		fflush(stdout);
		execvp("test2", args);
		perror("exec");
		/*
		int generation = -1;
		char last = 0xFF;
		char *s;
		while (1) {
			s=start;
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
		*/
    }
    return 0;
}
