#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <libelf.h>


#ifndef SOLARIS
#error "This is the Solaris agent.  Sorry"
#endif

int main(int args, char ** argv) {

	
	if (args < 2) {
		puts ("need command");
		return 1;
	}

	if (!strcmp(args[1], "attach")) {
		attach(args, argv);
	}

	char * fname = argv[1];
	errno = 0;
	unsigned int value;
	unsigned int offset;
	if (!(offset = strtol(argv[2], 0, 16)) && errno) {
		perror("offset");
		return 1;
	}
	errno = 0;
	if (!(value = strtol(argv[3], 0, 16)) && errno) {
		perror("value");
		return 1;
	}

	int fd = open(fname, O_RDWR);
	if (fd < 0) {
		perror(fname);
		return 1;
	}

	if (lseek(fd, offset, SEEK_SET) == (off_t) -1) {
		perror("lseek");
		return 1;
	}

	if (write(fd, (void *) &value, 4) != 4) {
		perror("write");
		return 1;
	}
	close(fd);
	return 0;
}
