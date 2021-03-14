# dependencies

An example of compiling multiple "packages" with multiple levels of dependencies.
This example demonstrates the ability to compile dependent modules without having to locate the source files of the dependencies.
All that is required is the pre-compiled artifacts.

The graph of direct dependencies looks like:

```
    app
    / \
   /   \
  /     \
 v       v
io      i18n
  \     /
   \   /
    \ /
     v
   string
```

This example also shows off using the `--include` flag.
Due to the way `purs-compile` works,
we don't actually have to specify the transitive dependencies.
Instead,
we only need to specify direct dependencies.

This example uses [bazel][], but could also be compiled straight from the terminal.

[bazel]: https://bazel.build/
