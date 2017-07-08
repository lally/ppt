all: rdpmc minimal-client
rdpmc: rdpmc.c
	g++ -o rdpmc -g -fpermissive -std=gnu++14 rdpmc.c -L/home/lally/src/perfmon2-libpfm4/lib -lpfm

minimal-client: minimal-client.cc ppt-Minimal.cc ppt-Minimal.hh
	g++ -o minimal-client -g -fpermissive -std=gnu++14 minimal-client.cc ppt-Minimal.cc -L/home/lally/src/perfmon2-libpfm4/lib -lpfm

ppt-Minimal.hh: minimal.spec
	../.stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/ppt generate ./minimal.spec


