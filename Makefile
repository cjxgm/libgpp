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
build/gpp: src/libgpp.cpp build/
	$(CXX) -o $@ $< $(CXXSTD) $(CXXFLAGS)

%/:
	@mkdir -p $@

