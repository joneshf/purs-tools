module Hello
  ( IO
  , main
  ) where

foreign import data IO :: Type -> Type

foreign import println :: String -> IO {}

main :: IO {}
main = println "Hello, world!"
