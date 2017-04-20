#ifndef INCLUDE_PPT_CONTROL_H
#define INCLUDE_PPT_CONTROL_H

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>

struct ppt_control {
    size_t                 control_blk_sz;
    uint64_t               data_block_hmem;
    uint64_t               data_block_hmem_attached;
    void*                  data_block;
    size_t                 data_block_sz;
    uint32_t               nr_perf_ctrs;
    struct perf_event_attr counterdata[3];
    uint64_t               client_flags;
};

#endif
