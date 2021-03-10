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

### Bintray

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

[github releases]: https://github.com/joneshf/purs-tools/releases
[purescript]: https://www.purescript.org/
