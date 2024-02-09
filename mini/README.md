# mini

mini is a language which targets the virtual machine in this repo. its goal is
to be small, as the name suggests, but also feature-complete enough to
demonstrate the capabilities of the vm well.

## goals

- [x] parse, typecheck, lower, and execute code
- [x] function calls
- [ ] type system with proper systems language features
  - [x] proper alignment and sizing
  - [x] basic primitives (unit, bool, ints, floats)
  - [ ] pointers
  - [ ] structured types
- [ ] structured control flow
- [ ] fleshed-out math and logic
- [ ] native ffi for scripting and small apps

## the language

mini is s-expression based, but it is *not* a lisp. I chose s-expressions simply
because they are easy to parse and have incredibly unambiguous semantics. the
semantics of mini is basically just C with an emphasis on expressions over
statements.

```lisp
# <- comments start with hash/octothorpe

# programs in mini are a series of declarations with `def`:
# (def <name> <type> <content>)
(def zero i32 0)
(def epsilon f32 0.00001)

# all executable code exists in a function, created with a `lambda` expression.
# function types are specified with the `->` operator:
(def square
  # the last type is the return type. the others specify parameter types.
  (-> i64 i64)
  # this lambda squares a number. because of the simple top-down type inference,
  # you could replace the `i64`s in function type with any kind of primitive
  # number and it would still compile and run as expected :)
  (lambda (x) (* x x)))
```
