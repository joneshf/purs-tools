{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Error
  ( Error (..),
    exit,
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
-- Decide how to exit based on which error we've got.
-- We basically only exit success if there are only warnings.
-- It might seem like we need more information to determine whether warnings should be treated as errors or not.
-- But if a warning should be an error,
-- it should be made into an error instead of masquerading around as a warning.
exit ::
  forall a env.
  ( HasLogFunc env
  ) =>
  LogSource ->
  Error ->
  RIO env a
exit source error = case error of
  Error.AllErrors {} -> do
    logErrorS source (display error)
    exitFailure
  Error.AllWarnings {} -> do
    logWarnS source (display error)
    exitSuccess
  Error.ErrorsAndWarnings {} -> do
    logErrorS source (display error)
    exitFailure

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
