module Baz
  ( baz
  , module Bar
  ) where

import Bar (foo) as Bar

baz :: Int
baz = Bar.foo
