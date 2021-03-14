# re-exports

An example of compiling multiple modules with re-exports.
This example demonstrates:
1. Re-exports still work with `purs-compile.

We have a long chain of dependencies:

```
    Grault
     |
     |
     |
     v
    Corge
     |
     |
     |
     v
    Qux
     |
     |
     |
     v
    Baz (re-exports Foo from Bar)
     |
     |
     |
     v
    Bar (re-exports Foo)
     |
     |
     |
     v
    Foo
```

This example uses [bazel][], but could also be compiled straight from the terminal.

[bazel]: https://bazel.build/
