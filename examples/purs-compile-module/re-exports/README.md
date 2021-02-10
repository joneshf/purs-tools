# re-exports

An example of compiling multiple modules with re-exports.
This example demonstrates two things:
1. Re-exports still work with "signature" externs.
1. Cut-off still works with re-exports.

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

Due to the use of re-exports (and the way the compiler is currently implemented):
- `Bar` has to know about `Foo`.
- `Baz` has to know about `Bar`, and `Foo`.
- `Qux` has to know about `Baz`, `Bar`, and `Foo`.

The other modules that do not re-export don't continue to grow in size:
- `Corge` only has to know about `Qux`.
- `Grault` only has to know about `Corge`.

What this means in practice is that if there's a material change to something below `Qux` in the dependency chain–e.g. changing the type of an exported value from `Int` to `String`–it will propagate to all modules up to `Qux`.
Assuming `Qux` can handle the change without causing a material change in its "signature"–e.g. handling the type change internally without changing the type of an exported value–no modules above `Qux` need to be re-compiled.
In other words,
We have cut-off!
