#include "agent/platform-agent.h"

#ifdef PLAT_LINUX
#include <stdio.h>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

/*
  After many vulnerabilities in this area, the only way to access a
  process's address space is via ptrace() --- even if you access
  /proc/pid/mem, you still have to have ptrace'd it first to get read
  access.  See linux-kernel/fs/proc/base.c

  If you're getting access control errors, try:
  (1) look at /etc/sysctl.d/10-ptrace.conf.
  (2) getting root.
 */

const std::string plat_exec_pathname(pid_t pid) {
  char exe_path[512];
  sprintf(exe_path, "/proc/%d/exe", pid);
  return std::string(exe_path);
}

off_t plat_sym_loc(pid_t pid, const std::string& object, off_t start_addr) {
  return start_addr;
}

int plat_pid_address_space(pid_t pid) {
  if (ptrace(PTRACE_ATTACH, pid, 0, 0) != 0) {
    perror("PTRACE_ATTACH");
    return -1;
  }
  int unused_status;
  waitpid(pid, &unused_status, 0);
  return pid;
}

int plat_read_address(int unused, pid_t pid, off_t sym_off) {
  errno = 0;
  int ret =  ptrace(PTRACE_PEEKDATA, pid, (void *) sym_off, 0);
  if (ret == -1 && errno != 0) {
    perror("PTRACE_PEEKDATA");
    return -1;
  }
  return ret;
}

bool plat_write_address(int unused, pid_t pid, off_t sym_off, int value) {
  return ptrace(PTRACE_POKEDATA, pid, (void *) sym_off, value) == 0;
}

void plat_pid_close_address_space(int pid) {
  ptrace(PTRACE_DETACH, pid, 0, 0);
}
#endif
