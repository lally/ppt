#include <stdio.h>
#include <string.h>
#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>
#include <asm/unistd.h>
#include <perfmon/pfmlib.h>
#include <perfmon/pfmlib_perf_event.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>


uint64_t rdpmc(int c) {
   uint32_t a, d;
  __asm__ volatile("rdpmc" :  "=a" (a), "=d" (d) : "c" (c));
   uint64_t result = a;
   return result | (static_cast<uint64_t>(d) << 32);
}

int open_report_counter(const char *cntr, int parent_fd) {
  struct perf_event_attr pe;
  pfm_perf_encode_arg_t pfm_arg = {&pe, NULL, sizeof(pfm_perf_encode_arg_t), 0, 0, 0 };
  bzero(&pe, sizeof(pe));
  int pfm_ret = pfm_get_os_event_encoding(cntr, PFM_PLM3, PFM_OS_PERF_EVENT_EXT, &pfm_arg);
  if (pfm_ret != PFM_SUCCESS) {
    printf("Failed to get perf encoding for %s: %s\n", cntr, pfm_strerror(pfm_ret));
    exit(1);
  }
  pe.disabled = 0;
  pe.exclude_kernel = 1;
  pe.exclude_hv = 1;
  pe.read_format = PERF_FORMAT_GROUP;

  const char *primary_desc = NULL;
#define SHOW(N) case N: printf(#N ": ");
#define CFG(N) if (pe.config == N) { printf(#N); break; }
  switch(pe.type) {
  SHOW(PERF_TYPE_HARDWARE) {
    CFG(PERF_COUNT_HW_CPU_CYCLES);
    CFG(PERF_COUNT_HW_INSTRUCTIONS);
    CFG(PERF_COUNT_HW_CACHE_REFERENCES);
    CFG(PERF_COUNT_HW_CACHE_MISSES);
    CFG(PERF_COUNT_HW_BRANCH_INSTRUCTIONS);
    CFG(PERF_COUNT_HW_BRANCH_MISSES);
    CFG(PERF_COUNT_HW_BUS_CYCLES);
    CFG(PERF_COUNT_HW_STALLED_CYCLES_FRONTEND);
    CFG(PERF_COUNT_HW_STALLED_CYCLES_BACKEND);
    CFG(PERF_COUNT_HW_REF_CPU_CYCLES);
  } break;
  SHOW(PERF_TYPE_SOFTWARE) {
    CFG(PERF_COUNT_SW_CPU_CLOCK);
    CFG(PERF_COUNT_SW_TASK_CLOCK);
    CFG(PERF_COUNT_SW_PAGE_FAULTS);
    CFG(PERF_COUNT_SW_CONTEXT_SWITCHES);
    CFG(PERF_COUNT_SW_CPU_MIGRATIONS);
    CFG(PERF_COUNT_SW_PAGE_FAULTS_MIN);
    CFG(PERF_COUNT_SW_PAGE_FAULTS_MAJ);
    CFG(PERF_COUNT_SW_ALIGNMENT_FAULTS);
    CFG(PERF_COUNT_SW_EMULATION_FAULTS);
    CFG(PERF_COUNT_SW_DUMMY);

    /* This indicates one of the "generalized" hardware events
       provided by the kernel.  See the config field definition for
       more details. */
  } break;

  SHOW(PERF_TYPE_TRACEPOINT); break;
    /* This indicates a tracepoint provided by the kernel tracepoint
       infrastructure. */

  SHOW(PERF_TYPE_HW_CACHE) {
    /* This indicates a hardware cache event.  This has a special
       encoding, described in the config field definition. */
    const uint8_t perf_hw_cache_id = pe.config & 0xff;
    const uint8_t perf_hw_cache_op_id = (pe.config >> 8) & 0xff;
    const uint64_t perf_hw_cache_op_result_id = pe.config >> 16;
#define ID(N) if (perf_hw_cache_id == N) { printf(#N "/"); }
    ID(PERF_COUNT_HW_CACHE_L1D);
    ID(PERF_COUNT_HW_CACHE_L1I);
    ID(PERF_COUNT_HW_CACHE_LL);
    ID(PERF_COUNT_HW_CACHE_DTLB);
    ID(PERF_COUNT_HW_CACHE_ITLB);
    ID(PERF_COUNT_HW_CACHE_BPU);
    ID(PERF_COUNT_HW_CACHE_NODE);
#undef ID
#define OP_ID(N) if (perf_hw_cache_op_id == N) { printf(#N "/"); }
    OP_ID(PERF_COUNT_HW_CACHE_OP_READ);
    OP_ID(PERF_COUNT_HW_CACHE_OP_WRITE);
    OP_ID(PERF_COUNT_HW_CACHE_OP_PREFETCH);
#undef OP_ID
#define RES_ID(N) if (perf_hw_cache_op_result_id == N) { printf(#N); }
    RES_ID(PERF_COUNT_HW_CACHE_RESULT_ACCESS);
    RES_ID(PERF_COUNT_HW_CACHE_RESULT_MISS);
#undef RES_ID
  } break;
  SHOW(PERF_TYPE_RAW) printf("%llu (0x%llx)", pe.config, pe.config); break;
    /* This indicates a "raw" implementation-specific event in the
       config field. */
  SHOW(PERF_TYPE_BREAKPOINT) break;

  default:
    char nmbuf[512];
    snprintf(nmbuf, sizeof(nmbuf), "Dynamic PMU %x (%u)", pe.type, pe.type);
    primary_desc = nmbuf;
  }
#undef SHOW
  int fd = perf_event_open(&pe, 0, -1, parent_fd, 0);
  if (fd < 0) {
    fprintf(stderr, "Error opening leader %llx: %s\n", pe.config, strerror(errno));
    exit(1);
  }
  ioctl(fd, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);
  // Ugh, ok this seems necessary for the rdpmc instruction to not segfault.
  mmap(NULL, sysconf(_SC_PAGESIZE), PROT_READ, MAP_SHARED, fd, 0);

  return fd;
}
struct read_format {
  uint64_t nr;            /* The number of events */
  uint64_t values[3];
};

void read_counters(struct read_format* dest, int ecx[], int nr_cnt) {
  switch(nr_cnt) {
  case 3:  dest->values[2] = rdpmc(ecx[2]);
  case 2:  dest->values[1] = rdpmc(ecx[1]);
  case 1:  dest->values[0] = rdpmc(ecx[0]);
  default:
    break;
  }
}

volatile int extern_symbol = 0x62340;
/*
  Input: the program will take 1-3 perf counter names.  it will also
  take flags indicating which ones to read using rdpmc, and if so, via
  indexed or hardcoded numbers.

  Processing:
    - It will run those names through libpfm, and print out key parts of the structure.
    - It will then open them using perf_event_open
    - It will then read them using rdpmc using the mechanism specified.
 */

int main(int args, char **argv) {
  enum { cntIndex, cntSpecial };
  int opt, maxlen;
  char *counter_names[3];
  char *pre_sep_names = NULL ;
  char *saveptr = NULL, *token = NULL;
  int counter_read_mode[3] = { cntIndex, cntIndex, cntIndex };
  int ecx[3] = {0,0,0};
  int num_cycles = 10000;
  struct read_format rd_before, rd_after;
  pfm_initialize();

  while ((opt = getopt(args, argv, "c:i:s:n:h")) != -1) {
    switch (opt) {
    case 'c':
      pre_sep_names = optarg;
      break;
    case 'i': {
      int index = atoi(optarg);
      if (index < 1 || index > 3) {
        puts("-i N: N should be 1-3");
        exit(1);
      }
      counter_read_mode[index-1] = cntIndex;
    } break;
    case 's': {
      int index = atoi(optarg);
      if (index < 1 || index > 3) {
        puts("-s N: N should be 1-3");
        exit(1);
      }
      counter_read_mode[index-1] = cntSpecial;
    } break;
    case 'n': {
      num_cycles = atoi(optarg);
    } break;
    case 'h':
    default: {
      puts("usage: rdpmc [-c COUNTER1,COUNTER2,...] [-i N] [-s N] [-n N]");
      puts(" -i and -s: indexed or special mode for index N.  Repeatable.");
      puts(" -n N: work factor N");
      exit(1);
    } break;
    }
  }

  if (pre_sep_names == NULL) {
    puts("Need counter names (via -c).  Comma separated.");
    exit(1);
  }
  pre_sep_names = strdup(pre_sep_names);

  // reuse opt to be the number of counters.
  opt = 0;
  maxlen = -1;
  while (opt < 3 && (token = strtok_r(pre_sep_names, ",", &saveptr)) != NULL) {
    int len = strlen(token);
    counter_names[opt++]=token;
    if (len > maxlen)
      maxlen = len;
    pre_sep_names = NULL;
  }

  int parent_fd = -1;
  int ecx_idx = 0;
  for (int i = 0; i < opt; i++) {
    printf("   %d: %*s ", (i+1), maxlen+1, counter_names[i]);
    if (parent_fd == -1) {
      parent_fd = open_report_counter(counter_names[i], -1);
    } else {
      open_report_counter(counter_names[i], parent_fd);
    }
    printf("\n");
    if (counter_read_mode[i] == cntIndex) {
      ecx[i] = ecx_idx++;
    } else {
      ecx[i] = 0x80; // FIXME.
    }
  }

  read_counters(&rd_before, ecx, opt);
  // begin the cpu work loop
  for (int i = 0; i < num_cycles; i++) {
    extern_symbol ^= rand();
  }
  read_counters(&rd_after, ecx, opt);

  for (int i = 0; i < opt; i++) {
    printf("  %s: %lu\n", counter_names[i], rd_after.values[i]-rd_before.values[i]);
  }

}
