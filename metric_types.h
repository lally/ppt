#ifndef INCLUDE_METRIC_TYPES_H
#define INCLUDE_METRIC_TYPES_H
/*
 * A shared header between reading and writing processes.
 */
typedef unsigned long metrics_beta1_seqno_t;

struct metrics_beta1_frame {
	metrics_beta1_seqno_t seqno;
	double                beta1;
};

	
#endif

