// A test file for syntax & code generation.

// We can emit "C" or "C++", only C's tested, but it compiles in C++.
emit C
// syntax: "buffer" <buffer name> <bufsz> <writerate>
// This buffer's named "beta2", at runtime,  the default values
// are 128 elements, expecting a write rate of 4 Hz. (e.g. 32 seconds
// worth of data in the buffer simultaneously).
buffer beta2 128 4


frame framecount {
    int count;
    double xi;
    time start, end;
    int count2;
}

frame xs {
  double xi;
}
