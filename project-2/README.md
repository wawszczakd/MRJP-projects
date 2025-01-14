# Latte Compiler

This project is an implementation of the Latte compiler in Haskell. Latte is a
simple imperative language inspired by Java and designed for educational
purposes. The compiler (`latc_llvm`) currently supports:

- Type checking
- Code generation in LLVM

## Optimizations

The following optimizations have been implemented so far:
- SSA Form: The compiler uses Static Single Assignment form, avoiding `alloca`.
- Constant Propagation: Simple expressions are evaluated at compile time, and
constants are propagated throughout the code.
- Local Common Subexpression Elimination (LCSE): Redundant expressions within a
single block are identified and eliminated, ensuring that each expression is
computed only once and reused.

## Building and Running

To build the compiler, type:
```bash
make
```

### Running the Compiler

To compile a `.lat` file, type:
```bash
./latc_llvm <file.lat>
```
The compiler will perform type checking and then generate LLVM code.

You can also use the test.sh script as follows:
```bash
./test.sh <folder>
```
The script will prompt you to either compile every .lat file in `<folder>` or
run each compiled file within the same folder.
