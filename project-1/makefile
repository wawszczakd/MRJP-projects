all:
	cabal build
	cp dist-newstyle/build/*/*/instant-0.1.0.0/x/insc_llvm/build/insc_llvm/insc_llvm insc_llvm
	cp dist-newstyle/build/*/*/instant-0.1.0.0/x/insc_jvm/build/insc_jvm/insc_jvm insc_jvm

clean:
	cabal clean
	rm -f examples/*.ll examples/*.bc examples/*.j examples/*.class
	rm -f insc_llvm insc_jvm
