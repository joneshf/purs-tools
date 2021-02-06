{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Error
  ( Error (..),
    fromRebuildModule,
  )
where

import qualified "purescript" Language.PureScript.Errors
import "rio" RIO hiding (error)

-- |
-- Any errors we might run into that we want to return at the end of the program.
data Error
  = AllErrors
      { errors :: Language.PureScript.Errors.MultipleErrors
      }
  | AllWarnings
      { warnings :: Language.PureScript.Errors.MultipleErrors
      }
  | ErrorsAndWarnings
      { errors :: Language.PureScript.Errors.MultipleErrors,
        warnings :: Language.PureScript.Errors.MultipleErrors
      }

instance Display Error where
  display :: Error -> Utf8Builder
  display error = case error of
    AllErrors {errors} ->
      fromString (Language.PureScript.Errors.prettyPrintMultipleErrors Language.PureScript.Errors.defaultPPEOptions errors)
    AllWarnings {warnings} ->
      fromString (Language.PureScript.Errors.prettyPrintMultipleWarnings Language.PureScript.Errors.defaultPPEOptions warnings)
    ErrorsAndWarnings {errors, warnings} ->
      fromString (Language.PureScript.Errors.prettyPrintMultipleWarnings Language.PureScript.Errors.defaultPPEOptions warnings)
        <> newline
        <> fromString (Language.PureScript.Errors.prettyPrintMultipleErrors Language.PureScript.Errors.defaultPPEOptions errors)

-- |
-- Since the 'Language.PureScript.Errors.MultipleErrors' can be empty,
-- we want to not create an 'Error' if we don't actually have an error.
-- We do a bit of parsing to make sure we're going to create an error before dumping it in there.
fromRebuildModule ::
  forall a.
  Bool ->
  (Either Language.PureScript.Errors.MultipleErrors a, Language.PureScript.Errors.MultipleErrors) ->
  Either Error Utf8Builder
fromRebuildModule ignoreWarnings result = case result of
  (Left errors, warnings)
    | ignoreWarnings,
      Language.PureScript.Errors.nonEmpty errors,
      Language.PureScript.Errors.nonEmpty warnings -> do
      Left (ErrorsAndWarnings {errors, warnings})
    | ignoreWarnings,
      Language.PureScript.Errors.nonEmpty warnings -> do
      Left (AllWarnings {warnings})
    | Language.PureScript.Errors.nonEmpty warnings -> do
      Left (AllErrors {errors = errors <> warnings})
    | Language.PureScript.Errors.nonEmpty errors -> do
      Left (AllErrors {errors = errors})
    | otherwise -> do
      Right ""
  (Right _, warnings)
    | ignoreWarnings,
      Language.PureScript.Errors.nonEmpty warnings -> do
      Left (AllWarnings {warnings})
    | Language.PureScript.Errors.nonEmpty warnings -> Left (AllErrors {errors = warnings})
    | otherwise -> Right ""

-- |
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"
