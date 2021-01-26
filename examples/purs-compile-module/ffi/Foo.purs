module Foo
  ( foo
  ) where

foreign import _foo :: Int

foo :: Int
foo = _foo
