#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#define private public
#include "ppt-Minimal.hh"
#include <string.h>
#include <sys/ioctl.h>
#include <linux/perf_event.h>
#include <sys/mman.h>
#include <asm/unistd.h>
#include <perfmon/pfmlib.h>
#include <perfmon/pfmlib_perf_event.h>
#include <assert.h>
#include "ppt-control.h"

extern "C" const char* _ppt_json_Minimal;
namespace ppt { namespace Minimal {
// secret debugging interface.
ppt_control *get_ctrl_ptr();
}}  // namespace ppt::Minimal

int main(int args, char ** argv) {
   int acount=1000, bcount=2000, ccount=3000; 
   char * minimal = strdup(_ppt_json_Minimal);
   const char * orig = _ppt_json_Minimal;
   printf ("sizeof(ppt_control) = %lu\n",  sizeof(struct ppt_control));
   printf ("sizeof(int) = %lu\n", sizeof(int));
   printf ("_ppt_json_Minimal: %p = [[[ %s ]]]\n", _ppt_json_Minimal, _ppt_json_Minimal);
   printf("\n\n PID IS %d\n\n", getpid());
   printf("\n sizeof(Minimal{first}) is %lu\n\n", sizeof(ppt::Minimal::first));
   pid_t pid = getpid();
   int delay = 50;
   if (args > 1) {
      delay = atoi(argv[1]);
   }


   printf("Measuring instruction count for this printf\n");


   while (1) {
       usleep(delay * 1000);
       ppt::Minimal::first record;
       record.snapshot_duration_start();
       record.snapshot_events_start();
       usleep(10);
       if (orig != _ppt_json_Minimal) {
          printf("_ppt_json_Minimal CHANGED from %p to %p\n", orig, _ppt_json_Minimal);
       } else if (strcmp(minimal, _ppt_json_Minimal)) {
          printf("_ppt_json_Minimal CHANGED STRING TO %s\n", _ppt_json_Minimal);
       }
       record.a = 0xaaaa0000 + acount++;
       record.b = acount - record.a;
       record.c = 0xcccccccc;
       record.snapshot_events_end();
       record.snapshot_duration_end();
       record.save();
       ppt_control *ctrl = ppt::Minimal::get_ctrl_ptr();
       printf("\r%d: %p %lu: [%8d] a:[%8d] hmem:%d d0=%lu d1=%lu d2=%lu (ctrl=%p) (d0_r=%lu, d1_r=%lu, d2_r=%lu)", pid, ppt::Minimal::data_Minimal::ppt_buf,
              sizeof(ppt::Minimal::first), record.__ppt_seqno, acount, ppt::Minimal::_ppt_hmem_Minimal,record.events_0_end - record.events_0_start, 
              record.events_1_end - record.events_1_start,  record.events_2_end - record.events_2_start,  ctrl,
              record.events_0_end, record.events_1_end, record.events_2_end);
       fflush(stdout);
   }
   return 0;
}
