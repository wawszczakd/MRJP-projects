all:
    cabal build
    cp dist-newstyle/build/x86_64-linux/ghc-9.0.2/instant-0.1.0.0/x/insc_llvm/build/insc_llvm/insc_llvm insc_llvm
    cp dist-newstyle/build/x86_64-linux/ghc-9.0.2/instant-0.1.0.0/x/insc_jvm/build/insc_jvm/insc_jvm insc_jvm

clean:
    cabal clean
    rm -f examples/*.ll examples/*.bc
    rm insc_llvm insc_jvm