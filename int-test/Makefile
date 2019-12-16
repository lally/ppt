PPT=stack exec ppt

cpptest: minimal.spec cpptest.cc
	$(PPT) generate minimal.spec
	$(CXX) -g -o $@ cpptest.cc ppt-Minimal.cc

