module IO
  ( IO
  , argv
  , bind
  , discard
  , exec
  , exit
  , forEach
  , println
  ) where

foreign import data IO ::
  Type ->
  Type

foreign import argv ::
  IO (Array String)

foreign import bind ::
  forall a b.
  IO a ->
  (a -> IO b) ->
  IO b

foreign import discard ::
  forall a.
  IO {} ->
  ({} -> IO a) ->
  IO a

foreign import exec ::
  String ->
  IO String

foreign import exit ::
  forall a.
  Int ->
  IO a

foreign import forEach ::
  forall a.
  Array a ->
  (a -> IO {}) ->
  IO {}

foreign import println ::
  String ->
  IO {}
