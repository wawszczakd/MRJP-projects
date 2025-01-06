all: latc_llvm

latc_llvm: runtime
	cabal build
	cp dist-newstyle/build/*/*/latte-0.1.0.0/x/latc_llvm/build/latc_llvm/latc_llvm latc_llvm

runtime: lib/runtime.bc

lib/runtime.bc: lib/runtime.ll
	llvm-as -o lib/runtime.bc lib/runtime.ll

lib/runtime.ll: lib/runtime.c
	clang -S -emit-llvm -o lib/runtime.ll lib/runtime.c

clean:
	cabal clean
	rm -f latc_llvm
	find . -type f \( -name "*.bc" -o -name "*.ll" \) -exec rm -f {} +

dist: clean
	tar -czvf dw440014.tar.gz *
