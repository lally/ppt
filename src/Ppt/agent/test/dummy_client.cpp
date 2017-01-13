/* A client to act as a placeholder for the LLVM-generated code.
   Echos all arguments and a (time_t) timestamp to a file named "client_test" */

#include <sys/types.h>
#include <time.h>
#include <iostream>
#include <fstream> 

int main(int args, char **argv) { 

  time_t now = time(0);

  std::ofstream out("client_test");
  if (!out) {
    perror("client_test");
    return 1;
  }

  out << now << " ( ";
  for (int i=0; i<args; ++i) {
    out << argv[i] << " ";
  }
  out << ")" << std::endl;

  return 1;
}

