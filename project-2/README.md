# Latte Compiler

This project is an implementation of the Latte compiler in Haskell. Latte is a
simple, imperative language inspired by Java, designed for educational use. The
compiler (`latc`) currently supports:

- Type checking (no code generation yet)

## Building and Running

To build the compiler, type:
```bash
make
```

### Running the Compiler

To compile a `.lat` file, type:
```bash
./latc <file.lat>
```
For now, it will only perform type checking.

There is also a script `run.sh` that can be used like that:
```bash
./run.sh <folder>
```
It will perform type checking on every `.lat` file from the folder.
