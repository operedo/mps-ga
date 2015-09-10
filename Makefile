#
# First, set dynamic lib path for ifort libomp
#
# Linux:   export LD_LIBRARY_PATH="/opt/intel/composerxe-2011/lib/intel64/:$LD_LIBRARY_PATH" 
# Mac  :   export DYLD_LIBRARY_PATH="/opt/intel/Compiler/11.1/080/lib:$DYLD_LIBRARY_PATH"
#

all: src tests

src:
	cd src; make;

tests:
	cd tests;           \
	make testmutation;  \
	make testHistogram; \
	make testHistogramOpenmp; \
	make testqsort;     \
	make testqsort2;     \
	make testcrossover; \
	make testcrossover2D; \
	make testfitness; \
	make testChainOperations; \
	make testBinaryTree; \
	make testHistogramChainOperations; \
	make testfitnessChainOperations; \
	make testrng;

clean:
	cd src; make clean
	cd tests; make clean

.PHONY: src tests clean
