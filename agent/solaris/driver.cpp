#include <unistd.h>

#include "attach.h"

int main(int args, char **argv) {
	if (args < 2) {
		puts ("need command");
		return 1;
	}

	if (!strcmp(args[1], "attach")) {
		return attach (args-2, argv+2);
	}
	else {
		puts("invalid command: commands are: 'attach'");
		return 1;
	}
}
