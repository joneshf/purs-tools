# purs-compile

A [PureScript][] compiler that works with pre-compiled artifacts.

## Motivation

The `purs compile` command that ships with the [PureScript][] compiler is a "whole world" compiler.
"Whole world" meaning that it needs every source module in order to compile anything.
This works well when you want to compile everything all at once from source.
This works pretty poorly when you want to use other pre-compiled artifacts.
This `purs-compile` binary exists to solve the problem of compling with other pre-compiled artifacts.

Hopefully, the merit of this idea can be realized and upstreamed into `purs compile`.

## Installation

### GitHub Releases

Currently, pre-compiled binaries for Linux and macOS are available on [GitHub Releases][]:

### Other

Other methods of installation will be documented in the future.

## Usage

For the most part, the interface is the same as `purs compile`.
This is intentional;
`purs-compile` is meant to be a drop-in replacement for `purs compile`.
Anything that currently works with `purs compile` should also work with `purs-compile`.

### Compiling without dependencies

If we have a few modules to compile and we don't have any dependencies:

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

we can compile them all in a batch.

```Console
$ purs-compile Bar.purs Baz.purs Foo.purs
Compiling Foo
Compiling Bar
Compiling Baz
```

This is the same as with `purs compile`.

### Compiling with "packages"

If we have a bunch of PureScript modules we'd like to compile as a "package,"
we can also do that and further depend on the pre-compiled artifacts.

Let's say we have a module that depends on the `purescript-prelude` package:

```PureScript
module Foo
  ( foo
  ) where

import Prelude

foo :: Int
foo = 1 + 2
```

We can compile `purescript-prelude` first:

```Console
$ purs-compile --output output-prelude '.spago/prelude/v4.1.1/src/**/*.purs'
Compiling Type.Data.RowList
Compiling Type.Data.Row
Compiling Record.Unsafe
Compiling Data.NaturalTransformation
Compiling Data.Boolean
Compiling Data.Symbol
Compiling Control.Semigroupoid
Compiling Control.Category
Compiling Data.Show
Compiling Data.Void
Compiling Data.Unit
Compiling Data.Semiring
Compiling Data.HeytingAlgebra
Compiling Data.Semigroup
Compiling Data.Ring
Compiling Data.BooleanAlgebra
Compiling Data.Eq
Compiling Data.CommutativeRing
Compiling Data.Ordering
Compiling Data.EuclideanRing
Compiling Data.Ord.Unsafe
Compiling Data.Ord
Compiling Data.DivisionRing
Compiling Data.Field
Compiling Data.Function
Compiling Data.Bounded
Compiling Data.Monoid
Compiling Data.Functor
Compiling Control.Apply
Compiling Control.Applicative
Compiling Control.Bind
Compiling Control.Monad
Compiling Prelude
Compiling Data.Monoid.Endo
Compiling Data.Semigroup.First
Compiling Data.Monoid.Additive
Compiling Data.Monoid.Conj
Compiling Data.Monoid.Multiplicative
Compiling Data.Semigroup.Last
Compiling Data.Monoid.Disj
Compiling Data.Monoid.Dual
```

Then we can compile our module with the pre-compiled `purescript-prelude` artifacts:

```Console
$ purs-compile --include output-prelude Foo.purs
Compiling Foo
```

N.B. It's important to note that since we're using pre-compiled dependencies,
we do not require all of the transitive sources passed to `purs-compile`.

[github releases]: https://github.com/joneshf/purs-tools/releases
[purescript]: https://www.purescript.org/
[purescript package]: https://hackage.haskell.org/package/purescript
