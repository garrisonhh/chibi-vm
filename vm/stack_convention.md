# virtual machine execution

the stack usage and calling convention is somewhat modeled after real x86 call
conventions.

## implementation

when calling, the caller...
1. pushes arguments in order, each should be 8-byte aligned
2. pushes the stack base and return location (also 8-byte aligned)
3. jumps to the function label

the callee can then reserve as much stack as required, and parameter values
can be accessed by peeking at the parameter location relative to the

when returning, the callee...
1. pops the return value
2. sets the stack top to the stack base
3. pops the previous stack base and return location and overwrites the program
   counter and stack top with these values
4. drops the arguments
5. pushes the return value

because of the way this is set up, this means that pushing/popping between
function entry and exit is completely possible and sane. which is what the vm
does to compute everything.

## example

the stack looks like this after entering the first function:

| offset      | data               |
| ----------- | ------------------ |
| 0x00 - 0x10 | base + return loc  |
| 0x10 - 0x18 | local memory       |

if this function calls a second function with three arguments, the stack would
look like this:

| offset      | data                     |
| ----------- | ------------------------ |
| 0x00 - 0x10 | base + return loc 0      |
| 0x10 - 0x18 | local memory             |
| 0x18 - 0x20 | parameter 0              |
| 0x20 - 0x28 | parameter 1              |
| 0x28 - 0x30 | parameter 2              |
| 0x30 - 0x38 | base + return loc 1      |
| 0x38 - 0x40 | local memory             |