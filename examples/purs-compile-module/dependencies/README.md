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

Unfortunately,
since each PureScript module has to know all the externs of every single one of its transitive dependencies,
we don't get any sort of cut-off when a material change happens to a module.
I.e. even though `Qux` directly depends on `Bar` and `Baz`,
we still need the externs of `Foo` in order to compile `Qux`.
The graph of transitive dependencies looks like:

```
    Qux
    /|\
   / | \
  /  |  \
 v   |   v
Bar  |  Baz
  \  |  /
   \ | /
    \|/
     v
    Foo
```

This example uses [bazel][], but could also be compiled straight from the terminal.

[bazel]: https://bazel.build/
