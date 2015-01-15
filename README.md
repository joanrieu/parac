# ParaC
ParaC is a small C-like compiler.

## Features
- Emits x86 asm (32 bit code).
- Has OpenMP-style parallel loops.
- Uses SSE for floating point arithmetic.
- Follows CDECL (ie the output is compatible with GCC-compiled C code).
- Support library includes `printint` and `printfloat` functions.
- Nice error messages with errors underlined in the source

## How it works
- Uses ANTLR4.
- Compiles in one pass.
- Can buffer asm output to reorder it if needed.
- Pushes every expression value on the stack.
- Uses pthreads for parallel loops.

## How to use
To compile a program:

    ./ParaC.sh sample.c >sample.s
    gcc -m32 sample.s src/ParaC.c -lpthread
    ./a.out
    
Use the `test.sh` script to do that in one command:

    ./test.sh sample.c # creates a.out.s then a.out and runs a.out

## Future improvements
- Avoid successive push/pop inside an expression.
- Enable 64 bit mode.
- String support
