#ifndef INCLUDE_PPT_CONTROL_H
#define INCLUDE_PPT_CONTROL_H

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>

/*
 * This struct is variable length.  'nr_perf_counters' indicates the length
 * of 'counterdata'.  It may be 0. 'ctrl_blk_sz' will be set appropriately.
 */
struct perf_counter_entry {
  uint32_t rcx;
  struct perf_event_attr event_attr;
};

struct ppt_control {
    size_t                 control_blk_sz;
    uint64_t               data_block_hmem;
    uint64_t               data_block_hmem_attached;
    void*                  data_block;
    size_t                 data_block_sz;
    uint64_t               client_flags;
    uint32_t               nr_perf_ctrs;
    struct perf_counter_entry counterdata[1];
};

#endif
