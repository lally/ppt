
Using PPT
=========

Lally Singh (lally@vt.edu)
BSD License

PURPOSE
-------

ppt generates a minimal raw-data instrumentation library and reader
program for capturing and storing measurements.  It works to minimize
intrusion into system performance and is intended to work with very
high rates of data.

The measurements are (abstractly) represented in a table, with each
column being a different measurement, and each row a synchronized
'frame' of values.  For example, a single iteration of a main loop
would be a single frame, containing one table cell per measurement.

Using some macros, a data structure is filled with measurement data
and that structure is written to disk.  

INSTALLATION
------------

The requirements for building ppt aren't trivial.  Sorry.

You'll need:
  ghc - The Glorious Glasgow Haskell Compilation System  6.12+
  llvm - The Low-Level Virtual Machine (tested with version 0.8)
  and a variety of add-on packages for ghc:
   - HStringTemplate
   - llvm (a haskell wrapper API for llvm above) (tested version 0.9.0.1)
   - Crypto
   - MissingH

  I strongly recommend starting with the Haskell Platform 
  (http://hackage.haskell.org/platform/) and then using 
  'cabal install' to get the add-on packages.

  llvm 

CONSTRAINTS
-----------

The primary frame data storage is a global variable, a structure
optimized to minimize the number of cache lines read per measurement
store and frame-save.

HOW TO USE
----------

### SETUP

Run 'ppt init' to quickly make a ./.ppt directory.

### FILE FORMAT

Create a file named 'beta2.spec':

    // A test file for syntax & code generation.
    
    // We can emit "C" or "C++", only C's tested, but C output compiles in C++.
    emit C
    // syntax: "buffer" <buffer name> <bufsz> <writerate>
    // This buffer's named "beta2", at runtime,  the default values
    // are 128 elements, expecting a write rate of 4 Hz. (e.g. 32 seconds
    // worth of data in the buffer simultaneously).  Currently ignored.
    buffer beta2 128 4
    
    
    frame framecount {
        int count;
        double xi;
        time start;
        time end;
        int count2;
    } 
    
    frame xs {
      double xi;
    }
    

While we're just using 'count' here, any number of elements can be in here,
with the following types: int, float, double, and time.  The first three map to
their C equivalents, the last maps to a struct timeval (handy for
gettimeofday()).  Currently, the 'emit C' is required, but we only support C
source generation.  They will be C++ compatible.

### GENERATION

Then call 'ppt generate beta2.spec'.  It will generate (internally, in .ppt/) a
header file, source code, and a llvm bitcode generator program.  We currently 
generate C and LLVM bitcode versions of the listener program.  Either way, you'll
have to build it yourself.

### RETRIEVAL

Then, call 'ppt retrieve beta2.spec'.  This will copy the files generated into
the local directory.

### INSTRUMENTATION

See test_writer_beta2.cpp for full details, but the gist is:
(1) #include the generated header ("beta2.h" here)
(2) Use the generated macros.  The macros are named as such:
            WRITE_FRAMECOUNT_COUNT();
            WRITE_FRAMECOUNT_XI();
            WRITE_FRAMECOUNT_START();
            WRITE_FRAMECOUNT_END();
            WRITE_FRAMECOUNT_COUNT2();
            WRITE_XS_XI();
    Just feed a parameter of the proper type (as we mentioned above) to it as the only arg.
(3) When ready, write out the frame:
    ppt_write_framecount_frame()
    ppt_write_xs_frame()
(4) Link against the generated base source ("beta2.c" here)

Note that the macros and write-functions use static instances of the frame
types (e.g.  there's a static instance of the struct generated for 'framecount'
and for 'xs').  For multithreaded code, you can do your own thread-locking, or,
much more appropriately, bug me to finalize the design for ppt to generate
multithreaded code.  If your write rate is low enough that you don't care about
contention (e.g. < 1KHz), and have all your data local and ready-to-go, the
thread-locking approach should be good for a handful (e.g. < 10) of contending
threads.


### PREPARATION

Next, build your listener.  We'll use the C version:

    gcc -o beta2_listen beta2_listen.c

Note that the name has to be beta2_listen (namely, the buffer name with _listen
added to the end.  It has to be in the current directory (hey, this is all
rather hackish, don't act surprised) when you call ppt below.

### MEASUREMENT

Get the pid of your running program (that calls the macros & was linked against
the generated C file (e.g. beta2.c)), say it was 3443.  Call ppt:

    ppt attach -p 3443 beta2.spec

This will call ppt-agent (make sure it's in the path), and ppt-agent will call
beta2_listener.  Let it record.  It'll store (by default, and
currently-immutable setting) into a file called 'out.buf' (yup, it's
terrible.).

Even worse, to stop recording, you have to kill the listener:

    pkill beta2_listen

But that's the worst of it.

### DATA CONVERSION

Finally, build and use the generated beta2_convert.c program to make CSVs of your data:

    g++ -o beta2_convert beta2_convert.c

./beta2_convert out.buf some_prefix

And you'll get two files:

some_prefix_framecount.csv
some_prefix_xs.csv

It'll have one column per field (two for 'time', tv_sec and tv_usec), and one
more sequence number up front.  That sequence number indicates order and lost
records (but you have to merge the sets between
the various files).
