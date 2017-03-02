# Buffer Specification Syntax

## Example

```
// First line is always the emit command.
emit C++;

// Buffer name and default buffer size.
buffer name 512;

// options go here

// Frame declarations after
frame simple {
  int a, b, c;
  double d;
  interval time duration;
  interval int seqnos;
}

frame complex {
   interval counter perfctrs;
   int array_sz;
}
```

## Emit Line
The `emit` line specifies a language.  Only `C++` is currently supported.

## Options

```
option time timeval;
option time timespec <realtime|realtime_course|monotonic|monotonic_course|monotonic_raw|boottime|proc_cputime|thread_cputime>;
option runtime multithread <true|false>;
option tag key value;
```

### Time Options
For timestamps (of type `time` in frames), there are two primary system calls available:

  - `gettimeofday(2)` - which works on `struct timeval`, microsecond precision
  - `clock_gettime(2)` - which works on `struct timespec`, nanosecond precision

You specify the type of structure to use, and that determines which system call the `ppt`-generated
code uses.  `clock_gettime(2)` takes an additional parameter.  That is specified in the last
argument to the option.  These correspond to the `clk_id` argument.  Note that `ppt` does not check
if the system used is new enough to support the given clock.


### Runtime options

The only runtime option supported is `multithread`, which generates code that supports writing from
multiple threads.  The non-multithread code is faster.

### Tags

Tags are user-defined arbitrary strings embedded within the generated code.  `ppt` can print these
out.  Tags can be useful for storing metadata.  Tag keys and values can be uoted strings.  Use
double quotes only.

## Frame specifications

One or more `frame` specifications are last in the buffer specification.  They specify objects that
can be put in the buffer.  All frames in a buffer share the same buffer, and are padded to be the
same size.  Frame members can be:

  - `int`
  - `double`
  - `time`
  - `counter`

Or an `interval` of one of these types.
