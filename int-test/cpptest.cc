#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <asm/unistd.h>
#include <assert.h>
#include "ppt-Minimal.hh"

// Forced activity for testing the tooling.
static void do_a_work() {
  static int buffer[512];
  static int count = 0;
  for (int i = 0; i < 512; i++) {
	buffer[i] = buffer[i] ^ (i << count);
	buffer[i] = buffer[i] ^ ( (32 - i) >> count );
  }
  count++;
}

static void do_b_work() {
  static int buffer[512];
  static int count = 0;
  for (int i = 0; i < 512; i++) {
	buffer[i] = buffer[i] ^ (i << count) ^ 1;
	buffer[i] = buffer[i] ^ ( (32 - i) >> count );
  }
  count += 3;
}

static void do_c_work() {
  static int buffer[512];
  static int count = 0;
  for (int i = 0; i < 512; i++) {
	buffer[i] = buffer[i] ^ (i << count) ^ 2;
	buffer[i] = buffer[i] ^ ( (32 - i) >> count );
  }
  count += 7;
}

int main(int args, char ** argv) {
   pid_t pid = getpid();
   int delay = 10;
   if (args > 1) {
      delay = atoi(argv[1]);
   }

   int last_a = 0;
   int last_d = 0;
   const int acount = rand() % 50;
   const int bcount = rand() % 50;
   const int ccount = rand() % 50;
   const int dcount = rand() % 50;
   const int ecount = rand() % 50;
   const int fcount = rand() % 50;
   int loopCounter = 0;
   int firstCount = 0;
   int secondCount = 0;
   while (1) {
     loopCounter++;
     if (rand() % 2 == 0) {
       firstCount++;
       ppt::Minimal::first record;
       record.snapshot_duration_start();
       record.snapshot_events_start();
       usleep(10);
       record.a = last_a + acount;
       record.b = 0x4c;
       record.c = record.b + ccount;
       last_a = record.a;
       for (int a = 0; a < acount; a++)
       	   do_a_work();
       for (int b = 0; b < bcount; b++)
       	   do_b_work();
       for (int c = 0; c < ccount; c++)
       	   do_c_work();
       record.snapshot_events_end();
       record.snapshot_duration_end();
       record.save();
     } else {
       secondCount++;
       ppt::Minimal::second record;
       record.snapshot_foos_start();

       record.d = last_d + (rand() % 25);
       record.e = record.d + (rand() % 25);
       record.f = 0x30303720u;
       last_d = record.d;

       // reusing do_[abc]_work().
       for (int a = 0; a < dcount; a++)
       	   do_a_work();
       for (int b = 0; b < ecount; b++)
       	   do_b_work();
       for (int c = 0; c < fcount; c++)
       	   do_c_work();

       record.snapshot_foos_end();
       record.save();
     }
     if (false) // 0 == (loopCounter % 1000))
       printf("Firsts: %8d, Seconds: %8d, Iters: %d\n", firstCount, secondCount, loopCounter);
     usleep(delay);
   }
   return 0;
}
