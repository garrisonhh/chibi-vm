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