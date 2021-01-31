module IO
  ( IO
  , discard
  , println
  ) where

foreign import data IO :: Type -> Type

foreign import discard ::
  forall a.
  IO {} ->
  ({} -> IO a) ->
  IO a

foreign import println ::
  String ->
  IO {}
