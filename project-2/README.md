# Latte Compiler

This project is an implementation of the Latte compiler in Haskell. Latte is a
simple imperative language inspired by Java and designed for educational
purposes. The compiler (`latc_llvm`) currently supports:

- Type checking
- Code generation in LLVM

## Optimizations

The following optimizations have been implemented so far:
- Uses SSA (not utilizing `alloca` instructions).
- Calculates simple expressions and propagates constants.

For instance, the following code:
```c
int main() {
    int a = 2 + 2 * 2;
    int b = a + 3;
    int c = b + 1;
    printInt(c);
    return 0;
}
```
will compile to:
```llvm
define i32 @main() {
L0:
    call void @printInt(i32 10)
    ret i32 0
}
```

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
