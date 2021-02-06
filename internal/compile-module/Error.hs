{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Error
  ( Error (..),
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
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"
