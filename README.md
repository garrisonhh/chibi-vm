# chibi vm

a fork of [chibicc](https://github.com/rui314/chibicc) which targets a virtual
machine.

## motivation

the virtual machine is planned for use in [fluent](https://github.com/garrisonhh/fluent)
and probably other smaller language projects. the idea is that using an already
well-tested parser for a well-defined language operating at a similar level of
hardware abstraction should give me plenty of space to test ideas and figure
out edge cases. in pursuit of these goals, I chose chibicc for the small size,
readability, extensive documentation with the attached book, and requiring
basically zero build system.

### goals

the primary goal is to create a vm that is general enough to do basically any
computational task, simple enough to read and understand in a day or two, and
with an interface that is pleasant to hack on.

- [x] math with unsigned + signed integers
- [x] simple logic
- [x] pointers, arrays, structs
- [x] generic control flow
- [x] functions
- [x] well-defined call convention and stack mechanics
- [x] elf-like segmented process memory mapping
- [x] very good test coverage
- [x] native function bindings
- [x] linking multiple translation units, allowing for parallelism

### non-goals

- implementing all features of C
  - this is a proof of concept project. if I'm satisfied that the design is
    robust and will require minimal (if any) future changes, there's no point
    going further
  - supporting parts of C that I don't like and don't plan to include in my own
    language projects (goto, bitfields, explicit inline...) is a waste of time
    for my goals

## getting started

chibi-vm is built with zig 0.11.0. just `zig build` for a debug build or
`zig build -Doptimize=ReleaseFast` for a release build.

### usage

you can run chibi-vm to see a (hopefully helpful) list of subcommands to use.
`chibi-vm run` will let you interpret a C program.

### compiler tests

once the executable is built, you can run end-to-end tests with `./runtests.py`.
it is written in relatively generic python 3, and uses the wonderful [c-testsuite](https://github.com/c-testsuite/c-testsuite).
you can use the results of the script to see how well this project stacks up
against the big boys :)

### vm tests

there is also extensive testing of the bytecode vm internals. these tests can
be run with `zig build test`, and can be found in the source code itself.

## project structure

`chibi` contains the chibicc source which I have left untouched. only the parser
and source file mechanics are relevant to this project.

`cc` contains the c frontend, which is a thin wrapper over chibi that then
traverses the AST to generate vm code. since it's not the main product of this
project, this code is untested and not particularly well-vetted.

`vm` contains the virtual machine, and definitions for opcodes, translation
units, etc. this code is well tested and commented.

`tests` contains the c test suite. `runtests.py` will run a manually filtered
subset of this suite.
