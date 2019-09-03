all: minimal-client

# for the experimental 'rdpmc' target, you need a built checkout of perfmon2-libpfm4.
LIBPFM4=${HOME}/src/perfmon2-libpfm4
rdpmc: rdpmc.c
	g++ -o rdpmc -g -fpermissive -std=gnu++14 -I${LIBPFM4}/include rdpmc.c -L${LIBPFM4}/lib -lpfm

minimal-client: minimal-client.cc ppt-Minimal.hh ppt-Minimal.cc
	g++ -o minimal-client -g -fpermissive -std=gnu++14 minimal-client.cc ppt-Minimal.cc

ppt-Minimal.hh: minimal.spec
	stack exec ppt -- generate --option native_counter true ./minimal.spec


