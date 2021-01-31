module Bar
  ( bar
  ) where

import Baz as Baz
import IO as IO

bar :: IO.IO {}
bar = IO.do
  IO.println "bar"
  Baz.baz
