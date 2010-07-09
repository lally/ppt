
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
all: test1 test2 libmet_listener

libmet_listener: libmet_listener.o
	g++ -o $@ $^

test2: test2.o
	g++ -o $@ $^

test1: test1.o
	g++ -o $@ $^


