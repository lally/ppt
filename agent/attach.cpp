#include <gelf.h>
#include <libelf.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <sstream>
#include <iostream>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <signal.h>
#include <fcntl.h>
#include <utility>
#include <algorithm>
#include <map>
#include <list>
#include "platform-agent.h" 

/*
  attach  -- attach instrumentation to a process

  arguments (one-char for compat with stock non-gnu getopt(3c)):
    -p pid -- pid of process to attach
    -s sym -- symbol of shared memory
    -S sym -v value -- symbol containing version of instrumentation,
                       and the value it should be (aborts if invalid)
    -i logger -- instrumentation logger to run
    -# sz -- size of shared memory, in bytes to create
    -V -- be verbose
    -D -- debugging traces.

  attach will run until it receives a signal, or until either the pid
  or logger die.  In any case, it'll nuke the shared memory symbol,
  (optionally) kill the logger, and (optionally) wait for the shared
  memory to be unused before nuking it. 'logger' can be a quoted
  string with additional front arguments.

  Sadly, the gnu libelf doesn't seem to come with very much
  documentation.  The Solaris libelf (and gelf), however, do.  If you
  want docs to this, hit up:
  http://download.oracle.com/docs/cd/E19963-01/821-1467/6nmetn6ms/index.html

  Or whever suncle puts the gelf(3ELF) pages next.
 */

static bool s_verbose;
static bool s_trace;
static int s_obsvd_pid; // the pid of the observed process
static int s_logger_pdi; // the pid of the logging process

int PT_SOLARIS_AGENT_VERSION = 1024; 

std::ostream& TRACE() {
	static std::stringstream ignored;
	if (s_trace) {
	  return std::cerr << "> ";
	}
	else {
	  ignored.str().clear();
	  return ignored;
	}
}

std::ostream& VERBOSE() {
	static std::stringstream ignored;
	if (s_verbose) {
	  return std::cerr << "-  ";
	}
	else {
	  ignored.str().clear();
	  return ignored;
	}
}

static int child_pid;
static int shm_handle;
static int obsvd_fd;
static off_t shm_off;

static void shutdown(int sig) {
  int64_t zero = 0;
  // reopen the process, write the 'close' (zero) value, and disconnect.
  obsvd_fd = plat_pid_address_space(s_obsvd_pid);
  plat_write_address(obsvd_fd, s_obsvd_pid, shm_off, 0);
  plat_pid_close_address_space(obsvd_fd);
  kill(child_pid, SIGINT);
  shmctl(shm_handle, IPC_RMID, 0);
  child_pid = 0;
  shm_handle = 0;
  VERBOSE() << "child died.  exiting" << std::endl;
  exit(0);
}


int attach (int argc, char **argv) {
	int c;
	extern char *optarg;
	extern int optind, optopt;
	int retcode = 1;

	std::string  ver_sym_name, shm_sym_name, logger_name;
	GElf_Sym sym_version = {0}, sym_shm = {0};
	long ver_value = -1;
	int shm_sz = -1;
	bool has_ver = false;
	bool trace_elf = false;
	while ((c = getopt(argc, argv, "p:s:v:N:i:n:VDX")) != -1) {
	  switch (c) {
	  case 'p':  s_obsvd_pid = atoi(optarg);  break;
	  case 's':  shm_sym_name.assign(optarg); break;
	  case 'v':  ver_sym_name.assign(optarg); break;
	  case 'N':  ver_value = strtoll(optarg, 0, 0);  has_ver = true; break;
	  case 'i':  logger_name.assign(optarg);  break;
	  case 'n':  shm_sz = atoi(optarg);       break;
	  case 'V':  s_verbose = true;            break;
	  case 'D':  s_trace = true;              break;
	  case 'X':  trace_elf = true;            break;
	  }
	}
	VERBOSE() << "This agent's pid is " << getpid() << std::endl;
	VERBOSE() << "Observing pid " << s_obsvd_pid << std::endl;
	VERBOSE() << "Shared memory symbol '" << shm_sym_name << "'" << std::endl;
	VERBOSE() << "Version symbol '" << ver_sym_name << "'" << std::endl;
	VERBOSE() << "Version Value " << ver_value << std::endl;
	VERBOSE() << "Logger " << logger_name << std::endl;
	VERBOSE() << "Shared memory " << shm_sz << " bytes" << std::endl;
	VERBOSE() << "Verbose is " << (s_verbose ? "ON": "OFF") << std::endl;
	VERBOSE() << "Tracing is " << (s_trace ? "ON": "OFF") << std::endl;
	VERBOSE() << "ELF Tracing is " << (trace_elf ? "ON": "OFF") << std::endl;
	
	if (!has_ver || shm_sz <= 0) {
	  printf("More parameters needed: ver_value is %d (%x), shm_sz is %d\n",
		 ver_value, ver_value, shm_sz);
	  return 1;
	}

	if (elf_version(EV_CURRENT) == EV_NONE) {
		printf("Failed libelf init: %s\n", elf_errmsg(-1));
		return 1;
	}

	if (s_obsvd_pid <= 0) {
		puts("Need a valid pid to observe");
		return 1;
	}
	if (logger_name.empty()) {
		puts ("Need a logger to call.");
		return 1;
	}

	Elf * elfObserved;
	int obsvd_aout_fd;
	std::string exec_pathname = plat_exec_pathname(s_obsvd_pid);
	if ((obsvd_aout_fd = open(exec_pathname.c_str(), O_RDONLY, 0)) <= 0) {
	  perror (exec_pathname.c_str());
	  return 1;
	}

	if ((elfObserved = elf_begin(obsvd_aout_fd, ELF_C_READ, NULL)) == NULL) {
	  printf ("opened %s but failed to read elf executable: %s\n",
		  exec_pathname.c_str(), elf_errmsg(-1));
	  close (obsvd_aout_fd);
	  return 1;
	}

	Elf_Kind elfObservedKind = elf_kind(elfObserved);

	std::pair<Elf_Kind, const char*> s_elfkinds_init[] = {
		std::make_pair( ELF_K_AR, "ARchive" ),
		std::make_pair( ELF_K_NONE, "None" ),
		std::make_pair( ELF_K_ELF, "ELF Object")
	};

	std::map<Elf_Kind, const char *> s_elfkinds(s_elfkinds_init,
						    s_elfkinds_init + 3);

	/* most of this taken out of the gelf(3ELF) manpage */
	Elf_Scn *scn = NULL;
	GElf_Shdr shdr;
	Elf_Data  *data;
	int cur_value = -1;
	int count = -1;
	struct sigaction act;
	off_t version_off;

	if (elfObservedKind != ELF_K_ELF) {
		printf("Wrong type of object: %s\n", s_elfkinds[elfObservedKind]);
		goto fail;
	}

	while ((scn = elf_nextscn(elfObserved, scn)) != NULL) {
		gelf_getshdr(scn, &shdr);
		if (shdr.sh_type == SHT_SYMTAB) {
			break;
		}
	}

	if (!scn) {
		puts("Couldn't find a symbol table");
		goto fail;
	}
	
	data = elf_getdata(scn, NULL);
	count = shdr.sh_size / shdr.sh_entsize;
	for (int i=0; i<count; ++i) {
		GElf_Sym sym;
		gelf_getsym(data, i, &sym);
		char *symname = elf_strptr(elfObserved, shdr.sh_link, sym.st_name);
		if (trace_elf) {
		  TRACE() << "Found symbol " << symname << std::endl;
		}
		if (!strcmp(symname, ver_sym_name.c_str())) {
			sym_version = sym;
		}
		else if (!strcmp(symname, shm_sym_name.c_str())) {
			sym_shm = sym;
		}
	}

	if (!sym_version.st_value || !sym_shm.st_value) {
		puts("couldn't find one of the symbols needed");
		goto fail;
	}

	//
	// Open up the address space.
	TRACE() << "Opening up address space for pid " << s_obsvd_pid << std::endl;
	if ((obsvd_fd = plat_pid_address_space(s_obsvd_pid)) < 0) {
	  perror("address space for process");
	  goto fail;
	}

	version_off = plat_sym_loc(s_obsvd_pid, exec_pathname, sym_version.st_value);
	shm_off = plat_sym_loc(s_obsvd_pid, exec_pathname, sym_shm.st_value);
	
	// first, read the version symbol
	cur_value = plat_read_address(obsvd_fd, s_obsvd_pid, version_off);
	if (cur_value != ver_value) {
	  printf("FAIL: binary version mismatch.  Wanted 0x%x, got 0x%x\n",
		 ver_value, cur_value);
	  goto fail_postfd;
	}

	VERBOSE() << "Version check OK" << std::endl;

	//
	// OK.  Created the shared memory segment, inject the handle
	// into the observee, and call up the observer.
	//
	if ((shm_handle = shmget(IPC_PRIVATE, shm_sz, IPC_CREAT | 0777)) == -1) {
	  perror("shmget");
	  goto fail_postfd;
	}

	if (!plat_write_address(obsvd_fd, s_obsvd_pid, shm_off, shm_handle)) {
	  perror("set shmem");
	  goto fail_postfd;
	}

	// go back to the shared mem handle location.  The signal
	// handler will zero it out when shutting down.

	// note: defer SIGCLD until after we're done!
	act.sa_handler = shutdown;
	act.sa_sigaction = 0;
	sigemptyset(&act.sa_mask);
	sigaddset(&act.sa_mask, SIGCLD);
	act.sa_flags = 0;
	sigaction(SIGINT, &act, 0);

	plat_pid_close_address_space(s_obsvd_pid);
	printf ("Process closed, shared memory handle %d inserted\n",
		shm_handle);
	if (0 == (child_pid = fork())) {
	  // child
	  char logger_cmdline[2048];
	  sprintf(logger_cmdline, logger_name.c_str(), shm_handle);
	  printf("Executing '%s'\n", logger_cmdline);

	  // now, split the thing by spaces.
	  char *cmd_end = logger_cmdline + strlen(logger_cmdline);
	  char *args[64];
	  int argc = 0;
	  char * c = logger_cmdline;
	  args[0] = strtok(logger_cmdline, " ");
	  while ((args[++argc] = strtok(NULL, " "))) 
	    ;
	  for (int i=0; i<argc; ++i) {
	    printf("argv[%d] = '%s'\n", i, args[i]);
	  }
	  execvp(args[0], args);
	  perror(args[0]);
	  exit(1);
	}

	sleep(5); // wait 5 seconds for the child to notice we gave it shared mem
	// we're live.  now we're just around to get a shutdown signal.
	waitpid(child_pid, 0, 0);
	/*	while (child_pid) {
	  pause();
	  } */
	shutdown(0);

	// if we get here, there was no failure, so clean up and return 0.
	// if there was a goto, it's below this line, defaulting to the
	// prior value of 1.
	retcode = 0;
	goto fail; // we've already closed the address space.

 fail_postfd:
	plat_pid_close_address_space(s_obsvd_pid);
 fail:	
	elf_end(elfObserved);
	close(obsvd_aout_fd);
	return retcode;
}
