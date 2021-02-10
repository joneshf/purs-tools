# dependencies

An example of compiling a multiple modules with multiple levels of dependencies.
This example demonstrates the ability to compile dependent modules without having to locate the source files of the dependencies.
All that is required is the compiled externs files.

The graph of direct dependencies looks like:

```
    Qux
    / \
   /   \
  /     \
 v       v
Bar     Baz
  \     /
   \   /
    \ /
     v
    Foo
```

This example also shows off using the `--output-signature-externs-file` flag.
The externs file that's generated has "null" source annotations.
That means immaterial changes within a module should cause fewer recompilations.

E.g. If we change how we compute the value `Foo.foo` (something entirely internal to the module),
it shouldn't cause a recompilation beyond just that module.
If we make a material change to the `Foo` module (like changing the exports),
it will cause a recompile of downstream modules.

This example uses [bazel][], but could also be compiled straight from the terminal.

[bazel]: https://bazel.build/
