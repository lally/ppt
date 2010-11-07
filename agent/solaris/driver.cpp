#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <vector>
#include <algorithm>
#include <iterator>
#include "attach.h"

int main(int args, char **argv) {
	if (args < 2) {
		puts ("need command");
		return 1;
	}

	// Duplicate argument vector, take out the primary command.
	std::vector<char*> new_args(argv, argv + args);
	new_args.erase(++new_args.begin());
	
	if (!strcmp(argv[1], "attach")) {
		return attach (args-1, &new_args[0]);
	}
	else {
		puts("invalid command: commands are: 'attach'");
		return 1;
	}
}
