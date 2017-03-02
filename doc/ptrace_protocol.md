# Ptrace Protocol

`ppt` works by defining a series of symbols in generted code.  Then it
looks for these symbols when attaching to a running executable (the
*process*).  This document covers what the symbols are, what they
mean, how they're manipulated, and how we intend to make this protocol
safe against attaching to programs that have code generated from one
version of `ppt` with another version.

`ppt` generates code per-buffer.  Each buffer is independent.  In
fact, different buffers' generated code can coexist in the same same
executable, even if they were generated with different versions of
`ppt`.  This works by versioning each buffers' symbols independently.
This feature is quite useful when using libraries that have their own
`ppt1 buffers.

Every symbol `ppt` defines starts with `__ppt_`, then has the buffer
name, then another underscore.  Then it has its symbol name:

 - `_stat` for the status block.
 - `_hmem` for the shared memory handle
 - `_json` for the JSON descrpition of the buffer

## Attach Procedure

`ppt` hsa a multi-stage attach procedure.  The stages are:

1. Check the `_json` block to get ABI version.
2. Check the `_stat` field to verify that no other `ppt` is attached.
3. Write the `_hmem` block to start the attach.

## _JSON Block

The `_json` block is a read-only text field created by the generated
code.  It is a JSON object containing:

1. The ABI version, under the `abi` key.
2. The buffer definition, under the `buffer` key.
    1. The emit under the `buffer.emit` key
    2. The layed-out frames (namely, `Framelayout`) under the`'buffer.frames` key
3. The tags set in the buffer spec and at generation time, under the `tags` key.
4. Arbitrary additional data under the `metadata` key. This last set
   of data is completely determined by the version of `ppt` that
   generated the JSON.

Note that the `_json` block may show a different `buffer`
specification than what was originally input.  Some features in `ppt`
may be specified before they are actually supported.  The `_json`
block must describe the actual implementation generated, so it may
have to describe a less featureful buffer implementation than
originally specified.

## Status Block

`ppt` first checks The `_stat` block when trying to attach.  The check
is to make sure no other copies of `ppt` are attached to this buffer.
Even though `ptrace(2)` prevents multiple processes from attaching to
a PID, `ppt` doesn't stay attached to a process very long.  So, it has
to make its own mutual exclusion.

The `_stat` field has this definition, in ABI v1:
```C
struct ppt_stat {
   pid_t  ppt_agent_pid;  /* written by ppt, ignored by buffer code */
};
```

If the `ppt_agent_pid` field isn't zero, it prints an error and the
pid that's there.  It's up to the user to detemrine if it's safe to
attach with a flag (say `--force-attach`).

`ppt` will write its own pid to `ppt_agent_pid`.


## Shared Memory Blocks

`ppt` has two behaviors for the shared memory block, under an option:

```
option buffer resize <on|off>
```

A resizable buffer requires an additional memory access on the primary
write path.  For some workloads, the cost in unacceptable, and
oversizing the initial buffer, or sacrificing some data loss, is
preferable.  For this reason, the default is `off`.

In both cases, it uses this type:
```C
struct ppt_control {
    size_t                 control_blk_sz;       /* sizeof(ppt_control) */
    uint64_t               data_block_hmem;
    uint64_t               data_block_hmem_attached;
    void                  *data_block;           /* initialized by the generated code */
    size_t                 data_block_sz;        /* in elements */
    uint32_t               nr_perf_ctrs;         /* Valued [0,3] */
    struct perf_event_attr counterdata[3];
    uint64_t               client_flags;
};
#define CLIENT_FLAG_ATTACHED (1)
#define CLIENT_FLAG_COUNTERS_SETUP (2)
```

The generated code keeps all of its state within this structure.
`ppt` can view and print it for diagnostic analysis.  It also uses
this information in the *buffer-resize operation*.

In both cases, `ppt` sets the `_hmem` shared memory handle to the
shared-memory ID of the Control Block.  When the process next runs the
generated code, it will detect the changed `_hmem` entry and attach it
to its address space.  Whether it attaches a second shared memory
block depends on whether the generated code was in static or dynamic
buffer-size mode.  `ppt` follows the protocol specified in the `_json`
string.

### Static Size

`ppt` creates a single shared memory block when attaching.  The block
starts with a *Control Block* (a `struct ppt_control`) and then
potentially some padding, followed by the array of frames (The *Data
Block*).  The padding will adjust the address of the array start to be
a multiple of the frame size.

The `data_block_hmem` and `data_block_hmem_attached` members aren't used.

### Dynamic Resizing

`ppt` creates two shared memory blocks when attaching.  The first is
the fixed-size *Control Block*.  The second is a variable-length *Data
Block* array.

The `data_block_hmem` and `data_block_hmem_attached` members link the
Control Block to the Data Block.  This second linkage creates an
additional memory access over the Static Size case.

When `ppt` attaches, it creates the two shared memory blocks and sets
the handle of the data block to `data_block_hmem` before it sets
`_hmem` in the process.  `data_block_hmem_attached` is set to zero.
When the process attaches the control block, it will also attach the
data block identified by `data_block_hmem`.  It will then set:
`data_block_hmem_attached = data_block_hmem`.

When `ppt` decides to resize the buffer, it will set `data_block_hmem`
to the new shared memory identifier.  The process will notice that
`data_block_hmem_attached != data_block_hmem`
and attach `data_block_hmem`.

