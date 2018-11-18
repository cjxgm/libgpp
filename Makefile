CXX = g++
CXXFLAGS = -Wall -Wextra -O3
CXXSTD = -std=c++14

all: build/gpp
clean:
	rm -rf build/
rebuild:
	@$(MAKE) clean
	@$(MAKE) all
run: all
	printf "#eval 1+1\n\n" | build/gpp
build/gpp: src/gpp.cpp src/libgpp.hpp build/libgpp.o | build/
	$(CXX) -o $@ $< build/libgpp.o $(CXXSTD) $(CXXFLAGS)
build/libgpp.o: src/libgpp.cpp src/libgpp.hpp | build/
	$(CXX) -c -o $@ $< $(CXXSTD) $(CXXFLAGS)

%/:
	@mkdir -p $@

