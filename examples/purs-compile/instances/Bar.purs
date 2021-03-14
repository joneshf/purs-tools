module Bar
  ( bar
  ) where

import Foo as Foo

bar :: Int
bar = Foo.method 3
