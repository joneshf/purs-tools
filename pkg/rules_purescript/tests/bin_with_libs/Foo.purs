module Foo
  ( foo
  ) where

import Bar as Bar
import Baz as Baz
import IO as IO

foo :: IO.IO {}
foo = IO.do
  IO.println "foo"
  Bar.bar
  Baz.baz
