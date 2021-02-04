# purs-compile-module

A [PureScript][] compiler that works on a per-module level.

## Motivation

The `purs compile` command that ships with the [PureScript][] compiler is a batch compiler.
This works well when you want to compile everything all at once from source.
This works pretty poorly when you only want to compile a single module from source with other pre-compiled dependencies.
This `purs-compile-module` binary exists to solve the problem of compling a single module from source with other pre-compiled dependencies.

Given that attempting to manage a non-trivial amount of modules like this is really hard,
this `purs-compile-module` binary is not intended to be used directly by humans.
This is much more in the realm of something another tool builds on top of.

## Installation

### Bintray

Currently, pre-compiled binaries for Linux and macOS are available on [bintray][]:

### Other

Other methods of installation will be documented in the future.

## Usage

There are quite a few options to the binary,
but at its core you need to supply all of the inputs and all of the outputs.

### Single module with no dependencies

In the simple case of a single module with no dependencies:

```PureScript
module Foo
  ( foo
  ) where

foo :: Int
foo = 3
```

We can compile that module.

```Console
$ purs-compile-module --purs-file Foo.purs
Compiling Foo
```

This will compile `Foo.purs` and not output anything.
This example could be seen as a sort of typecheck-only workflow.

We can also generate an externs file that other modules can depend on.

```Console
$ purs-compile-module --output-standard-externs-file Foo.cbor --purs-file Foo.purs
Compiling Foo
```

This will compile `Foo.purs` and output a `Foo.cbor` externs file that other modules can depend on.

### A module with dependencies

If we have a couple of modules to compile:

```PureScript
module Foo
  ( foo
  ) where

foo :: Int
foo = 3
```

```PureScript
module Bar
  ( bar
  ) where

import Foo as Foo

bar :: Int
bar = Foo.foo
```

```PureScript
module Baz
  ( baz
  ) where

import Bar as Bar

baz :: Int
baz = Bar.bar
```

we can compile each of them.

```Console
$ purs-compile-module --output-standard-externs-file Foo.cbor --purs-file Foo.purs
Compiling Foo
$ purs-compile-module --input-externs-file Foo.cbor --output-standard-externs-file Bar.cbor --purs-file Bar.purs
Compiling Bar
$ purs-compile-module --input-externs-file Bar.cbor --input-externs-file Foo.cbor --output-standard-externs-file Baz.cbor --purs-file Baz.purs
Compiling Baz
```

N.B. It's important to note that even though we are compiling each module individually,
we still require all of the transitive dependencies of a single module.
This is currently a limitation of the way the underlying Haskell [purescript package][] works.
In the future, we might be able to work only with the direct dependencies.

### Compiling to "signature" externs

If we're using `purs-compile-module` to build tooling atop of,
and that tooling understands how to cut-off builds based on their outputs (e.g. [bazel][] or [shake][]),
we can produce externs files that are less susceptible to immaterial changes.
These are tentatively being called "signature" externs files,
but there's no precedent for what this type of externs file is called.

Let's say we have a file:

```PureScript
module Foo
  ( foo
  ) where

foo :: Int
foo = 3
```

We can compile that and produce both the "signature" and the "standard" externs files:

```Console
$ purs-compile-module --output-signature-externs-file Foo.1.signature.cbor --output-standard-externs-file Foo.1.standard.cbor --purs-file Foo.purs
Compiling Foo
```

We can make an entirely whitespace change:

```diff
diff --git a/Foo.purs b/Foo.purs
index 131b32c..70be141 100644
--- a/Foo.purs
+++ b/Foo.purs
@@ -3,4 +3,4 @@ module Foo
   ) where

 foo :: Int
-foo = 3
+foo =       3
```

Recompile it (using new files to output to):

```Console
$ purs-compile-module --output-signature-externs-file Foo.2.signature.cbor --output-standard-externs-file Foo.2.standard.cbor --purs-file examples/purs-compile-module/simple/Foo.purs
Compiling Foo
```

And then compare the externs files:

```Console
$ cmp Foo.1.signature.cbor Foo.2.signature.cbor
$ cmp Foo.1.standard.cbor Foo.2.standard.cbor
Foo.1.standard.cbor Foo.2.standard.cbor differ: char 72, line 1
```

It's largely irrelvant what is different.
The important part is that _something_ is different in the "standard" externs file whereas there's no different in the "signature" externs file.

What this means for tooling is that something like [bazel][] or [shake][] can cut-off a full rebuild if there are immaterial changes to a module deep in the hierarchy.
If there are material changes to a module,
the affected modules will still have to recompile.

[bazel]: https://bazel.build/
[bintray]: https://bintray.com/joneshf/generic/purs-compile-module/_latestVersion
[purescript]: https://www.purescript.org/
[purescript package]: https://hackage.haskell.org/package/purescript
[shake]: https://shakebuild.com/
