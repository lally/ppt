/* solaris-agent.cpp
   -----------------

   A solaris procfs implementation of the platform agent apis.
*/

#include "agent/platform-agent.h"

#ifdef PLAT_SOLARIS

#ifndef sun
#error "This only works on solaris man.  Sorry."
#endif

#include <stdio.h>

const std::string solaris_exec_pathname(pid_t pid) {
  return std::string("");
}

int plat_read_address(int handle, pid_t unused, off_t sym_off) {
  int cur_value;
  if (lseek(handle, sym_off, SEEK_SET) != sym_off
      || read(handle, &cur_value, sizeof(int)) != sizeof(int)) {
    perror("get versioninfo");
    return -1;
  }
  return cur_value;
}

bool plat_write_address(/* FIXME */) {
  return (lseek(obsvd_fd, shm_off, SEEK_SET) != shm_off
	  || write(obsvd_fd, &shm_handle, sizeof(int)) != sizeof(int);
}
const std::string plat_exec_pathname(pid_t pid) {
  char exe_path[512];
  sprintf(exe_path, "/proc/%d/exe", pid);
  return std::string(exe_path);
}

off_t plat_sym_loc(pid_t pid, const std::string& object, off_t start_addr) {
  return start_addr;
}

int plat_pid_address_space(pid_t pid) {
  char as_path[512];
  sprintf(as_path, "/proc/%d/mem", pid);
  puts(as_path);
  return open(as_path, O_RDWR, 0);
}
}



#endif
