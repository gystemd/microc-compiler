# microcc

 `microcc` is a compiler for the `microc` programming language, a subset of C. The compiler is written in OCaml and uses the LLVM API to generate the LLVM IR code. See file `report.pdf` for a more detailed description of the architecture and implemenation.

It supports the following features:
* (multi) variable declaration and initialization;
* if-then-else statements;
* while, do-while, for loops;
* functions (can only return `int`, `void` , `float`, `bool` or `char`);
* pointers and multi-dimensional arrays;
* structs;
* pre/post increment/decrement operators, i.e., `++` and `--`, and  abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
* `sizeof` and bitwise operators;
* floating point arithmetic and strings as in C, i.e. null-terminated arrays of characters;



### Runtime support

MicroC support library functions to perform I/O operations:
* `void print(int)` to print an integer on the standard output;
* `int getint()` to read an integer from the standard input.

Their implementation is written in C and it is the file `bin/rt-support.c`.


## Install with Docker
The easiest way to install the compiler is to use the provided Dockerfile.
To build the Docker image, run the following command:
```sh
$ docker build -t microcc .
```

Then you can run commands with `docker run`. For instance, to run all the tests you can use the following command:

```sh
$ docker run microcc ./test_all.sh test-custom
```

## Requirements to build the code
The code requires:
* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2
* llvm >= 12.0.0

You can install the required dependencies via `opam`
```sh
$ opam install menhir ppx_deriving llvm
```
[Here](https://github.com/ocaml-ppx/ppx_deriving), you can find the documentation of `ppx_deriving`.

[Here](https://llvm.moe/ocaml/), you can find the documentation of LLVM bindings.

## Building the code and running the tests
Typing `make` will generate a the compiler executable `bin/microcc.exe`, and the testing program `test/codegen_test.exe`:
```
$ make
```

You can compile and run a MicroC program with the following command:
```
dune exec bin/microcc.exe -- $1
clang a.bc bin/rt-support.c
./a.out
```

You can also use the bash script `test_all.sh` to test the result of your compilation. The usage of such script is:
```
$ ./test_all.sh <path/to/folder>
```
`samples/`, `test-custom` and `test-custom2` contain a set of test programs that you can use to test your compiler.

## Directory structure #

Read about the organization of the project [here](../SETUP.md#project-structure).

## The source code

The `lib/` directory contains the modules for each phase of the compiler. 
Your code will stay there.

More precisely, the `lib/` directory provides:

    ast.ml                       <-- Definition of the abstract syntax tree of MicroC 
    microcc.ml                   <-- The file from which build the executable 
    location.ml                  <-- The module Location provides two data types to represent code locations
    location.mli                 <-- The interface of the module Location   
    parser.mly                   <-- Menhir specification of the grammar
    parsing.ml                   <-- The module Parsing implements the parser
    parsing.mli                  <-- The interface of the module Parsing  
    scanner.mll                  <-- ocamllex specification of the scanner 
    scanner.mli                  <-- The interface of the module Scanner
    symbol_table.ml              <-- The module Symbol_table provides the implementation of a symbol table
    symbol_table.mli             <-- The interface of the module Symbol_table
    semantic_analysis.ml         <-- The module Semantic_analysis implements the semantic checker
    semantic_analysis.mli        <-- The interface of the module Semantic_analysis
    codegen.ml                   <-- The module Codegen translates a µcomp-lang into a LLVM module
    codegen.mli                  <-- The interface of the module Codegen
    optimizer.ml                 <-- The module Optimizer runs a sequence of optimization on a LLVM module
    optimizer.mli                <-- The interface of the module Optimizer

The `bin/` directory provide:

    microcc.ml                   <-- The code of the compiler executable
    mcompc.mli                   <-- A dummy interface
    rt-support.c                 <-- A simple implementation of the functions of the standard library

