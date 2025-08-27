# Compiler Construction Template

This repo contains the template code for the compiler construction course in the
summer term 2025. You will use this code to construct your own compiler in the
course exercises.

## Contents

* `include`: This directory contains the C header and sources files `runtime.h`
  and `runtime.c`. They contain runtime support code needed for interaction with
  the operating system, such as `read_int` and `print_int`. Do not modify these
  files.
* `project/`, `build.sbt`: These are used by sbt to configure the project. Do
  not modify these files.
* `examples/`: Contains examples in the source language to be compiled. For each
  example, there is a `*.lang` file containing the source code and `*.check`
  file containing the expected output after executing the binary obtained from
  compiling the corresponding source file. For each of the following exercises,
  we will provide you with additional pairs of `*.lang` and `*.check` files and
  your compiler should be able to produce an executable that prints the expected
  output. Do not modify these files.
* `src/test/scala/compiler/GoldenTests.scala`: Automatically compiles all
  `*.lang` files in the `examples/` folder, runs the produced executable, and
  asserts that the output is what the `*.check` file says. Do not modify this
  file.
* `src/test/scala/compiler/CompilerTests.scala`: Will contain additional tests
  for the compiler. Feel free to modify this file to test different parts of
  your compiler.
* `src/main/scala/compiler/`: This package will contain your own code. We have
  provided the `Main.scala` file with the function `main`, `compile` and
  `readFile`. Do not modify the signature of `compile` as it is called by the
  golden tests. You will have to modify its implementation. This function should
  convert the given S-Expression into your own AST, perform the compilation
  steps and generate a program in the `x86` AST (see below) from which we will
  then assemble the executable. If your compiler code works correctly you should
  see a new `*.exe` file in the `out/` directory created in the root directory
  of the repository, with a filename matching the input file (i.e.
  `examples/print_42.lang` will compile to `out/print_42.exe`).
* `src/main/scala/lang`: Contains the parser and lexer for the source language
  into S-Expressions. The output of the S-Expression parser is then the input to
  your `compile` function. In the following exercises, we will update the parser
  to support an extended source language. Do not modify this code.
* `src/main/scala/x86`: Contains the AST for the x86 assembler language which
  will then be compiled to an executable. Your `compile` function should
  generate an x86 program using this AST, and call the `assemble` function with
  that source tree. As with the parser, we will continually extend these
  definitions in the following exercises. Do not modify this code.

## Exercises

In your own Github classroom repository, you will get this template code to
build your compiler on top of. For each chapter of the lecture, your homework
will be to implement the different steps in the compiler pipeline, starting from
an S-Expression representing the source language as defined in the lecture. You
should work in a branch other than `main` and submit your work via a pull
request.

After each homework, we will update the template repository with an extended source
language and an extended target x86 definition. You should merge these updates
into your repository. Each homework will also come with new golden tests. Your
task is to make them compile and run successfully.

Since we will update both the `lang` and `x86` packages in this project, you
should never modify any code in these, otherwise tests and grading will fail.

## Installation

In order to compile and run the code in this repository you will need to install
the following:

* [Scala](https://www.scala-lang.org/)
* [Sbt](https://www.scala-sbt.org/)
* [Clang](https://clang.llvm.org/)

If you already know how to install these or already have them installed, you can
skip the rest.

### Linux

* Clang should be provided within the repositories of most major distributions,
  so you can simply install it with your usual package manager:
    * Ubuntu/Debian: `sudo apt install clang`
    * Arch: `sudo pacman -S clang`
    * Fedora: `sudo dnf install clang`
* First install [coursier](https://get-coursier.io/docs/cli-installation),
  following the instructions on the website. On some distributions, `curl` is
  not always installed, in this case you need to first install it using your
  package manager (e.g. `sudo apt install curl`). You can check if `curl` is
  installed correctly by running `curl --version`.
* Once coursier has been installed, install sbt using coursier from the command
  line: `cs install sbt`.
* Check if everything has been installed correctly, by running `sbt test` in the
  repository root directory.

### MacOS

* On Mac, clang is installed using Xcode: `xcode-select install`
* First install [coursier](https://get-coursier.io/docs/cli-overview) using the
  instructions on the website.
* Once coursier has been installed, install sbt using coursier from the command
  line: `cs install sbt`.
* Check if everything has been installed correctly, by running `sbt test` in the
  repository root directory.

### Windows

* Install [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) according
  to the installation instructions on the website. Perform all of the following
  steps and all of your development from within the WSL console.
* Install clang using apt: `sudo apt install clang`
* Install curl using apt: `sudo apt install curl`
* Install [coursier](https://get-coursier.io/docs/cli-overview) using the
  installation instructions on the website.
* Install SBT using coursier: `cs install sbt`.
* Check if everything has been installed correctly, by running `sbt test` in the
  repository root directory.
