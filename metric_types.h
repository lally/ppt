#ifndef INCLUDE_METRIC_TYPES_H
#define INCLUDE_METRIC_TYPES_H
#include <limits.h>
/*
 * A shared header between reading and writing processes.
 */
typedef unsigned long metrics_beta1_seqno_t;
enum { metrics_beta1_seqno_max = ULONG_MAX };

struct metrics_beta1_frame {
	metrics_beta1_seqno_t seqno;
	double                beta1;
};

	
#endif

