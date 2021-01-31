module BinWithLibs
  ( main
  ) where

import Foo as Foo
import IO as IO

main :: IO.IO {}
main = IO.do
  Foo.foo
