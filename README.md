# Educational Compiler

**Short summary**
This repository is a privately maintained continuation of the *Compiler Construction* course template used in the Summer Term 2025 for the **Compiler Construction course at the University of Tübingen**. It was originally distributed as a GitHub Classroom template for the lecture exercises and has been migrated to this personal repository so the compiler can be developed further as a portfolio / learning project.

**Important — transparency & attribution**

* This project began as course material for an educational assignment. Parts of the original code (small helper functions and test scaffolding) were written by course staff/tutors and are intentionally retained where useful.
* The repository preserves the `main` branch commit history relevant to the compiler, but housekeeping was applied during migration: commits authored by the GitHub Classroom bot have been rewritten/attributed to the current maintainer to avoid exposing bot addresses.

---

## Status

This is an educational compiler in active private development. The goal is to continue the course work into a polished portfolio project that demonstrates the author’s learning and engineering skills. The repository is **not** intended as a drop-in production compiler; it is a student project that is being shaped into a public showcase.

---

## Development & usage

### Build & test

From the repository root:

```bash
# run the tests (compiles examples and runs golden tests)
sbt test
```

If tests create executables, they will typically end up in `out/` (matching the input filename, e.g. `examples/print_42.lang` → `out/print_42.exe`).

### Typical workflow

1. Implement/modify compiler logic inside `src/main/scala/compiler/`.
2. Run `sbt test` and inspect failing tests / golden test diffs.
3. Use `examples/` as source of truth for expected behavior.

---

## Installation (kept / cleaned from the original template)

You need the following tools installed to build and run the project:

* [Scala](https://www.scala-lang.org/)
* [sbt](https://www.scala-sbt.org/) (sbt can be installed via coursier — see below)
* [Clang](https://clang.llvm.org/) (used by the runtime/assembly step)

### Linux

```bash
# example (Ubuntu/Debian)
sudo apt update
sudo apt install clang curl
# install coursier (follow get-coursier.io) and then:
cs install sbt
# then run tests:
sbt test
```

### macOS

```bash
# install Xcode command line tools (clang)
xcode-select --install
# install coursier (see get-coursier.io), then:
cs install sbt
sbt test
```

### Windows (recommended: WSL)

Install WSL and perform the Linux steps inside the WSL environment. Example:

```text
# In WSL:
sudo apt update
sudo apt install clang curl
# install coursier, then:
cs install sbt
sbt test
```
