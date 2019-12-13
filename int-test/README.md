Integration Tests
-----------------

In here lies a small integration test framework.  It:
- Generates C++ code from a `.spec` file
- Links that code against an existing `.cpp` file.
- Runs that executable, which generates frames.
- Attaches to that process for a few seconds, gathering data
- Decodes and verifies the data.

