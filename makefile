
CXXFLAGS += $(shell ../bin/llvm-config --cxxflags) -frtti -g -O0

LLHOME = $(shell cd .. && pwd)

.PHONY: all clean

all: sl

sl: simplisp.o
	$(CXX) -o $@ $^ $(shell ../bin/llvm-config --libs --ldflags)

clean:
	rm -f *.o sl

.PHONY: llvm-build
llvm-build:
	cd ../llvm-2.8 && ./configure --prefix=$(LLHOME) --enable-targets=x86 --enable-optimized
	$(MAKE) clean -C ../llvm-2.8
	$(MAKE) -C ../llvm-2.8
	$(MAKE) install -C ../llvm-2.8