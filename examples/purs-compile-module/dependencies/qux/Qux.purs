module Qux
  ( qux1
  , qux2
  ) where

import Bar as Bar
import Baz as Baz

qux1 :: Int
qux1 = Bar.bar

qux2 :: Int
qux2 = Baz.baz
