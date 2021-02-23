module Foo
  ( class Class
  , method
  ) where

class Class a where
  method :: a -> Int

instance classInt :: Class Int where
  method :: Int -> Int
  method int = int
