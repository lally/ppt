PPT=stack exec ppt

.PHONY: clean all

all: cpptest

clean: 
	$(RM) -f ppt-Minimal.* cpptest


cpptest: minimal.spec cpptest.cc
	$(PPT) generate minimal.spec
	$(CXX) -g -o $@ cpptest.cc ppt-Minimal.cc

