
CXXFLAGS=-I/usr/local/include -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -g -c $< -o $@
all: test1 test2 libmet_listener reader test_writer pt-solaris-agent test_attach dummy_client

#test_llvm 


libmet_listener: libmet_listener.o
	g++ -o $@ $^

test2: test2.o
	g++ -o $@ $^

test1: test1.o
	g++ -o $@ $^

reader: reader.o
	g++ -o $@ $^

test_writer: test_writer.o
	g++ -o $@ $^

test_llvm: test_llvm.o
	g++ -g -o $@ $^ `llvm-config --ldflags --libs` 

pt-solaris-agent: agent/solaris/driver.o agent/solaris/attach.o
	g++ -g -o $@ $^ -lelf 

test_attach: agent/test/test_attach.o
	g++ -g -o $@ $^

dummy_client: agent/test/dummy_client.o
	g++ -g -o $@ $^

ppt: Configuration.hs Generate.hs Listener.hs Main.hs SIParser.hs StaticInstrumentation.hs Storage.hs
	ghc -o ppt --make Main.hs

