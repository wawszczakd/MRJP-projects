# Instant Compiler

This project is an implementation of the Instant compiler in Haskell.

## Overview

Instant is a simple programming language created for the MRJP (Methods of
Programming Languages Realization) course.
It consists of only two types of instructions:
 * **Expressions** — print their evaluated value,
 * **Assignments** — set a variable’s value with `variable = expression`.

This repository contains LLVM and JVM compilers for the Instant programming
language.

## Building and Running

To build the project, simply type:
```bash
make
```

### Running the JVM Compiler

To run the JVM compiler on a file, type:
```bash
./insc_jvm <file.ins>
```

This command will create `<file.j>` and `<file.class>` files. `<file.j>`
contains the generated JVM code, and `<file.class>` can be run with:
```bash
java <file>
```

### Running the LLVM Compiler

To run the LLVM compiler on a file, type:
```bash
./insc_llvm <file.ins>
```

This command will create `<file.ll>` and `<file.bc>` files. `<file.ll>` contains
the generated LLVM code, while `<file.bc>` can be executed with:
```bash
lli <file.bc>
```
