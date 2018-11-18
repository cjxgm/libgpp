CC = gcc
CCFLAGS = -Wall -Wextra -O3

all: build/gpp
clean:
	rm -rf build/
rebuild:
	@$(MAKE) clean
	@$(MAKE) all
run: all
	printf "#eval 1+1\n\n" | build/gpp
build/gpp: src/gpp.c build/
	$(CC) -o $@ $< $(CCFLAGS)

%/:
	@mkdir -p $@

