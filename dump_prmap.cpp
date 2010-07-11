#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <procfs.h>
#include <inttypes.h>
#include <assert.h>
#include <vector>
#include <iterator>
#include <utility>


void print_item(const prmap_t& p) {
	int f = p.pr_mflags;
	char flagbuf[64];
	sprintf(flagbuf,
			"%c%c%c%c%c%c%c",
			f & MA_READ? 'r' : '-',
			f & MA_WRITE? 'w' : '-',
			f & MA_EXEC? 'x' : '-',
			f & MA_SHARED? 's' : '-',
			f & MA_ISM? 'i' : '-',
			f & MA_NORESERVE? 'n' : '-',
			f & MA_SHM? 'm' : '-');
			
	printf("%08x %08x %64s %8s %016llx %08x %08x\n",
		   p.pr_vaddr,
		   p.pr_size,
		   (p.pr_mapname && strlen(p.pr_mapname))? p.pr_mapname : "-",
		   flagbuf,
		   p.pr_offset,
		   p.pr_pagesize,
		   p.pr_shmid);
}

int main(int args, char ** argv) {
	FILE * fp;
	int retcode = -1;
	if (args < 2) {
		puts("need filename");
		return 1;
	}
	if (NULL == (fp = fopen(argv[1], "rb"))) {
		perror(argv[1]);
		return 1;
	}

	struct stat filestat;
	retcode = stat(argv[1], &filestat);
	assert(!retcode);
	
	off_t filesz = filestat.st_size;
	if (filesz % sizeof(prmap_t) != 0) {
		printf("WARNING: File size is %d, not a multiple of %d.\n",
			   filesz, sizeof(prmap_t));
	}
	size_t nitems = filesz / sizeof (prmap_t);

	std::vector<prmap_t>  items(nitems);
	fread((void*) &items[0], sizeof(prmap_t), nitems, fp);

	printf("%8s %8s %64s %8s %16s %8s %8s\n",
		   "vaddr", "size", "mapname", "mflags", "offset", "pagesz",
		   "shmid");
	for_each(items.begin(), items.end(), print_item);
	fclose(fp);
	return 0;
}
