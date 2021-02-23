# instances

An example of compiling multiple modules with typeclass instances.
This example demonstrates two things:
1. Typeclass instances still work with "signature" externs.

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

Due to the use of typeclasses (and the way the compiler is currently implemented):
- Every module implicitly passes along any instances from its explicit dependencies.
