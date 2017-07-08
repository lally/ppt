#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#define private public
#include "ppt-Minimal.hh"
#include <string.h>
#include <sys/ioctl.h>
#include <linux/perf_event.h>
#include <asm/unistd.h>
#include <perfmon/pfmlib.h>
#include <perfmon/pfmlib_perf_event.h>
#include <assert.h>
#include "ppt-control.h"

extern "C" const char* _ppt_json_Minimal;
namespace ppt {
ppt_control *get_ctrl_ptr();
}
/*
static long
perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
                int cpu, int group_fd, unsigned long flags)
{
    int ret; 
    ret = syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
    return ret;
}*/

uint64_t rdpmc(int c) {
   uint32_t a, d;
  __asm__ volatile("rdpmc" :  "=a" (a), "=d" (d) : "c" (c));
   uint64_t result = a;
   return result | (static_cast<uint64_t>(d) << 32);
}
/*
struct ppt_control {
    size_t                 control_blk_sz;
    uint64_t               data_block_hmem;
    uint64_t               data_block_hmem_attached;
    void*                  data_block;
    size_t                 data_block_sz;
    uint32_t               nr_perf_ctrs;
    struct perf_event_attr counterdata[3];
    uint64_t               client_flags;
};*/

int main(int args, char ** argv) {
   int acount=1000, bcount=2000, ccount=3000; 
   char * minimal = strdup(_ppt_json_Minimal);
   const char * orig = _ppt_json_Minimal;
   pfm_initialize();
/*
typedef struct {
    struct perf_event_attr *attr;
    char **fstr;
    size_t size;
    int idx;
    int cpu;
    int flags;
} pfm_perf_encode_arg_t;
*/
   printf ("sizeof(ppt_control) = %lu\n",  sizeof(struct ppt_control));
   printf ("sizeof(int) = %lu\n", sizeof(int));
   printf ("_ppt_json_Minimal: %p = [[[ %s ]]]\n", _ppt_json_Minimal, _ppt_json_Minimal);
   printf("\n\n PID IS %d\n\n", getpid());
   printf("\n sizeof(Minimal{first}) is %lu\n\n", sizeof(ppt::first));
   pid_t pid = getpid();
   int delay = 50;
   if (args > 1) {
      delay = atoi(argv[1]);
   }
   long long count, prior;

   int fd;
   struct perf_event_attr pe;
   pfm_perf_encode_arg_t pfm_arg = { &pe, NULL, sizeof(pfm_perf_encode_arg_t), 0, 0, 0 };
   memset(&pe, 0, sizeof(struct perf_event_attr));
   int pfm_ret = pfm_get_os_event_encoding((args > 1)? argv[1] : "LLC_MISSES", PFM_PLM3, PFM_OS_PERF_EVENT_EXT, &pfm_arg);
   if (pfm_ret != PFM_SUCCESS) {
       printf("Failed to get perf encoding: %s\n", pfm_strerror(pfm_ret));
       return -1;
   }
   pe.disabled = 0;
   pe.exclude_kernel = 1;
   pe.exclude_hv = 1;

   fd = perf_event_open(&pe, 0, -1, -1, 0);
   if (fd == -2) {
      fprintf(stderr, "Error opening leader %llx\n", pe.config);
      exit(EXIT_FAILURE);
   }

   ioctl(fd, PERF_EVENT_IOC_RESET, -1);
   ioctl(fd, PERF_EVENT_IOC_ENABLE, -1);

   printf("Measuring instruction count for this printf\n");

   read(fd, &prior, sizeof(long long));

   while (1) {
//       read(fd, &v, sizeof(long long));

       uint64_t v = rdpmc(0);
//       uint64_t v = 0;
       read(fd, &count, sizeof(long long));
       usleep(delay * 1000);
       ppt::first record;
       record.snapshot_duration_start();
       usleep(10);
       if (orig != _ppt_json_Minimal) {
          printf("_ppt_json_Minimal CHANGED from %p to %p\n", orig, _ppt_json_Minimal);
       } else if (strcmp(minimal, _ppt_json_Minimal)) {
          printf("_ppt_json_Minimal CHANGED STRING TO %s\n", _ppt_json_Minimal);
       }
       record.a = 0xaaaa0000 + acount++;
       record.b = count - prior;
       record.c = 0xcccccccc;
       record.snapshot_duration_end();
       record.save();
       ppt_control *ctrl = ppt::get_ctrl_ptr();
       printf("\r%d: %p %lu: [%8d] a:[%8d] %d (pc=%llu %llu) (pv=%ld %llu) (ctrl=%p cntrs=%d)", pid, ppt::data_Minimal::ppt_buf,
              sizeof(ppt::first), record.__ppt_seqno, acount, ppt::_ppt_hmem_Minimal, count - prior, count, v, count - v,
              ctrl, (ctrl == nullptr)? 0:ctrl->nr_perf_ctrs);
       prior = count;
       fflush(stdout);
   }
    close(fd);
   return 0;
}
