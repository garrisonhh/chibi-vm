# chibi vm

a fork of [chibicc](https://github.com/rui314/chibicc) which targets a virtual
machine.

## getting started

chibi-vm is built with zig 0.11.0. just `zig build` for a debug build or
`zig build -Doptimize=ReleaseFast` for a release build.

### usage

you can run chibi-vm to see a (hopefully helpful) list of subcommands to use.
`chibi-vm run` will let you interpret a C program.

### compiler tests

once the executable is built, you can run end-to-end tests with `./runtests.py`.
it is written in relatively generic python 3, and uses the wonderful [c-testsuite](https://github.com/c-testsuite/c-testsuite). you can use the results of the
script to see how well this project stacks up against the big boys :)

there is also extensive testing of the bytecode vm internals. these tests can
be run with `zig build test`, and can be found in the source code itself.

## motivation

the virtual machine is planned for use in [fluent](https://github.com/garrisonhh/fluent)
and probably other smaller language projects. the idea is that using an already
well-tested parser for a well-defined language operating at a similar level of
hardware abstraction should give me plenty of space to test ideas and figure
out edge cases. in pursuit of these goals, I chose chibicc for the small size,
readability, extensive documentation with the attached book, and requiring
basically zero build system.