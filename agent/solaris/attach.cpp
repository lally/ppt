#include <libelf.h>
#include <unistd.h>
#include <stdlib.h>
#include <gelf.h>
#include <string>
/*
  attach  -- attach instrumentation to a process

  arguments (one-char for compat with stick getopt(3c)):
    -p pid -- pid of process to attach
	-s sym -- symbol of shared memory
	-S sym -v value -- symbol containing version of instrumentation,
	                   and the value it should be (aborts if invalid)
	-i logger -- instrumentation logger to run
	-# sz -- size of shared memory, in bytes to create
	-V -- be verbose
	-D -- debugging traces.

  attach will run until it receives a signal, or until either the pid
  or logger die.  In any case, it'll nuke hte shared memory symbol,
  (optionally) kill the logger, and (optionally) wait for the shared
  memory to be unused before nuking it.
 */

static bool s_verbose;
static bool s_trace;
static int s_obsvd_pid; // the pid of the observed process
static int s_logger_pdi; // the pid of the logging process

std::ostream& TRACE() {
	static std::stringstream ignored;
	if (s_trace) {
		return std::stderr;
	}
	else {
		ignored.str().clear();
		return ignored;
	}
}

std::ostream& VERBOSE() {
	static std::stringstream ignored;
	if (s_verbose) {
		return std::stderr;
	}
	else {
		ignored.str().clear();
		return ignored;
	}
}

int attach (int argc, char **argv) {
	int c;
	extern char *optarg;
	extern int optind, optopt;
	int retcode = 1;

	std::string  ver_sym_name, shm_sym_name, logger_name;
	GElf_Sym sym_version = {0}, sym_shm = {0};
	int ver_value = -1;
	int shm_sz = -1;
	while ((c = getopt(argc, argv, "p:s:S:v:i:#:VD")) != -1) {
		switch (c) {
		case 'p':  s_obsvd_pid = atoi(optarg);  break;
		case 's':  shm_sym_name.assign(optarg);	break;
		case 'S':  ver_sym_name.assign(optarg); break;
		case 'v':  ver_value = atoi(optarg);    break;
		case 'i':  logger.assign(optarg);       break;
		case '#':  shm_sz = atoi(optarg);       break;
		case 'V':  s_verbose = true;            break;
		case 'D':  s_trace = true;              break;
		}
	}
	if (elf_version(EV_CURRENT) == EV_NONE) {
		printf("Failed libelf init: %s\n", elf_errmsg(-1));
		return 1;
	}

	if (s_obsvd_pid <= 0) {
		puts("Need a valid pid to observe");
		return 1;
	}
	if (logger.empty()) {
		puts ("Need a logger to call.");
		return 1;
	}

	ELF * elfObserved;
	const char observed_pathname[512];
	sprintf(observed_pathname, "/proc/%d/object/a.out", s_obsvd_pid);
	int obsvd_aout_fd;
	if ((obsvd_aout_fd = open(observed_pathname, O_RDONLY, 0)) <= 0) {
		perror (observed_pathname);
		return 1;
	}

	if ((elfObserved = elf_begin(obsvd_aout_fd, ELF_C_READ, NULL)) == NULL) {
		printf ("opened %s but failed to read elf executable: %s\n",
				observed_pathname, elf_errmsg(-1));
		close (obsvd_aout_fd);
		return 1;
	}

	ELF_Kind elfObservedKind = elf_kind(elfObservedKind);

	static std::pair<ELF_Kind, const char*> s_elfkinds_init[] = {
		{ ELF_K_AR, "ARchive" },
		{ ELF_K_NONE, "None" },
		{ ELF_K_ELF, "ELF Object" }
	};

	static std::map<ELF_Kind, const char *> s_elfkinds(s_elfkinds_init,
													   s_elfkinds_init + 3);
	if (elfObservedKind != ELF_K_ELF) {
		printf("Wrong type of object: %s\n", s_elfkinds[elfObservedKind]);
		goto fail;
	}

	/* most of this taken out of the gelf(3ELF) manpage */
	Elf_Scn *scn = NULL;
	GElf_Shdr shd;
	Elf_Data  *data;

	while ((scn = elf_nextscn(elf, scn)) != NULL) {
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
	int fd_as;
	char as_path[512];
	sprintf(as_path, "/proc/%d/as", s_obsvd_pid);
	if ((fd_as = open(as_path, O_RDWR, 0)) <= 0) {
		perror(fd_as);
		goto fail;
	}

	
	// first, read the version symbol
	int cur_value = -1;
	if (read(fd_as, &cur_value, sizeof(int), (off_t) sym_version.st_value) != sizeof(int)) {
		goto fail_postfd;
		perror("read");
	}

	if (cur_value != ver_value) {
		printf("FAIL: binary version mismatch.  Wanted 0x%x, got 0x%x\n",
			   ver_value, cur_value);
		goto fail_postfd;
	}

	//
	// OK.  Created the shared memory segment, call up the observer,
	// and inject the handle into the observee.
	//

	
	
	// if we get here, there was no failure, so clean up and return 0.
	// if there was a goto, it's below this line, defaulting to the
	// prior value of 1.
	retcode = 0;
	
 fail_postfd:
	close(fd_as)
 fail:	
	elf_end(elfObservedKind);
	close(obsvd_aout_fd);
	return retcode;
}
