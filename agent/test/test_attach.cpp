/* Test the agent's ATTACH facility.

   Return 0 if succeeded.  Return 1 on failure */



/* We'll run the agent on ourself, and verify that it injects us with
   the right symbol values */


#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <vector>
#ifndef AGENT_BINARY_NAME
#ifdef sun
#define AGENT_BINARY_NAME "pt-solaris-agent"
#endif
#endif

#ifndef AGENT_BINARY_NAME
#error "AGENT_BINARY_NAME is unknown, we don't know how to invoke the agent"
#endif

int AGENT_VERSION;
int AGENT_MEMORY_HANDLE;
pid_t SELF;

static char buffer[512];
/* Call our agent with the specified 'format' for arguments.
   We split them with basic quoting rules (no substitution) */
void call_agent(char * format, ...) {
	va_list args;
	va_start(args, format);
	vsprintf(buffer, format, args);
	std::vector<char*> res_args;
	res_args.push_back(AGENT_BINARY_NAME);
	char *c = buffer;
	char quote = 0;
	char *start = buffer;
	while (*c) {
		if (quote) {
			if (*c == quote) {
				*c = 0;
				res_args.push_back(start);
				start = c+1;
			}
		}
		else if (*c == '\'' || *c == '\"') {
			quote = *c;
			start = c+1;
		}
		else {
			if (*c == ' ') {
				*c = 0;
				res_args.push_back(start);
				start = c+1;
			}
		}
		c++;
	}
	if (start != c) {
		res_args.push_back(start);
	}
	res_args.push_back(0);
	pid_t child;
	if (child = fork()) {
		int status;
		printf("%d: Waiting for child %d\n",
			   SELF, child);
		waitpid(child, &status, 0);
	}
	else {
		execvp(AGENT_BINARY_NAME, &res_args[0]);
		perror(AGENT_BINARY_NAME);
		return;
	}
}

int main(int args, char **argv) {
	SELF = getpid();
	/* FAILURE MODE TESTS */
	srand(time(0));
	AGENT_VERSION = rand();

	call_agent("attach -p %d -s %s -S %s -v %d -i %s -# %d -V -D",
			   SELF, "AGENT_MEMORY_HANDLE",
			   "AGENT_VERSION",  AGENT_VERSION,
			   "dummy_client", 16384);

	puts("test_attach exiting");
	return 0;
}
