#include <stdio.h>
#include <unistd.h>

volatile int value;

int main(int args, char ** argv) {

	while (1) {
		printf ("The symbol 'value' is at 0x%08x and is %d\n",
				&value, value);
		sleep(10);
	}

	return 0;
}
