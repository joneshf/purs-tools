module Bar
  ( bar
  , module F
  ) where

import Foo (foo) as F

bar :: Int
bar = 3
