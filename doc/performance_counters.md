# Performance Counter Support
We have performance counter support, mostly for intel.

## Native Counters
What's tricky is that we *also* have support for native `rdpmc`
support, which is substantially faster.  BUT, it's quite flaky, with
the nasty effect of segfaulting the program if it doesn't like the
counter you asked for.

Cons:

 - It doesn't seem to like native x86arch counters.  These are
   counters that are always on and enabled on Intel (maybe AMD?) cpus.

 - It can't work for non-cpu perf counters.  So perf::PAGE_FAULTS and
   perf::CONTEXT_SWITCHES won't work in native mode.

Pros:
 - Really fast.  24-40 cpu cycles per counter instead of the cost of a syscall (overall).

## Open Questions

1. Do we even enable native counter support?  It's flaky and only
useful for a subset of counters.

2. How do we size the number of counters we provide storage for in the
frame?  There are two factors at work here:

   a. Are we actually limited to 3 perf counters, or is that just CPU
   counters?  Could we ask for more when we ask for non-CPU counters
   like CONTEXT_SWITCHES?

   b. If we enabled architectural counters (and it was stable enough
   for use), then we can count those even when we have 3 other
   counters enabled (for rdpmc).

