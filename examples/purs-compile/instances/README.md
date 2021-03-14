# instances

An example of compiling multiple modules with typeclass instances.
This example demonstrates two things:
1. Typeclass instances still work with `purs-compile`.

We have a chain of dependencies:

```
    Baz (uses a value that uses an instance from Foo)
     |
     |
     |
     v
    Bar (uses an instance from Foo)
     |
     |
     |
     v
    Foo (defines an instance)
```

This example uses [bazel][], but could also be compiled straight from the terminal.

[bazel]: https://bazel.build/
