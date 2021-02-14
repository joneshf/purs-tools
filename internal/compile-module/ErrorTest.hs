{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ErrorTest
  ( exit,
    fromRebuildModule,
  )
where

import qualified "compile-module" Error
import qualified "hedgehog" Hedgehog
import qualified "hedgehog" Hedgehog.Gen
import qualified "hedgehog" Hedgehog.Main
import qualified "hedgehog" Hedgehog.Range
import qualified "purescript" Language.PureScript.Errors
import "rio" RIO hiding (error)
import qualified "base" System.IO

exit :: IO ()
exit = do
  System.IO.hSetEncoding stdout System.IO.utf8
  System.IO.hSetEncoding stderr System.IO.utf8
  Hedgehog.Main.defaultMain
    [ Hedgehog.checkParallel $$(Hedgehog.discoverPrefix "exitProp")
    ]

exitPropOnlyExits0ForAllWarnings :: Hedgehog.Property
exitPropOnlyExits0ForAllWarnings = Hedgehog.property do
  error <- Hedgehog.forAllWith showError genError
  result <- liftIO (try (runSimpleApp (Error.exit "ErrorTest" error)))
  case (error, result) of
    (Error.AllErrors {}, Left (ExitFailure 1)) -> Hedgehog.success
    (Error.AllWarnings {}, Left ExitSuccess) -> Hedgehog.success
    (Error.ErrorsAndWarnings {}, Left (ExitFailure 1)) -> Hedgehog.success
    (_, Left _) -> Hedgehog.failure
    (_, Right _) -> Hedgehog.failure

fromRebuildModule :: IO ()
fromRebuildModule = do
  System.IO.hSetEncoding stdout System.IO.utf8
  System.IO.hSetEncoding stderr System.IO.utf8
  Hedgehog.Main.defaultMain
    [ Hedgehog.checkParallel $$(Hedgehog.discoverPrefix "fromRebuildModuleProp")
    ]

fromRebuildModulePropDoesNotCreateErrorsOrWarnings :: Hedgehog.Property
fromRebuildModulePropDoesNotCreateErrorsOrWarnings = Hedgehog.property do
  ignoreWarnings <- Hedgehog.forAll Hedgehog.Gen.bool
  case Error.fromRebuildModule ignoreWarnings (Right (), mempty) of
    Left _ -> Hedgehog.failure
    Right _ -> Hedgehog.success

fromRebuildModulePropFiltersEmptyErrorsAndWarnings :: Hedgehog.Property
fromRebuildModulePropFiltersEmptyErrorsAndWarnings = Hedgehog.property do
  ignoreWarnings <- Hedgehog.forAll Hedgehog.Gen.bool
  case Error.fromRebuildModule ignoreWarnings (Left mempty, mempty) of
    Left _ -> Hedgehog.failure
    Right _ -> Hedgehog.success

fromRebuildModulePropPromotesWarningsToErrorsIfNotIgnoring :: Hedgehog.Property
fromRebuildModulePropPromotesWarningsToErrorsIfNotIgnoring = Hedgehog.property do
  errors <- Hedgehog.forAll genMultipleErrors
  warnings <- Hedgehog.forAll genNonEmptyMultipleErrors
  case Error.fromRebuildModule False (Left errors, warnings) of
    Left Error.AllErrors {} -> Hedgehog.success
    Left _ -> Hedgehog.failure
    Right _ -> Hedgehog.failure

fromRebuildModulePropKeepsWarningsIfIgnoring :: Hedgehog.Property
fromRebuildModulePropKeepsWarningsIfIgnoring = Hedgehog.property do
  errors <- Hedgehog.forAll genMultipleErrors
  warnings <- Hedgehog.forAll genNonEmptyMultipleErrors
  case Error.fromRebuildModule True (Left errors, warnings) of
    Left Error.AllErrors {} -> Hedgehog.failure
    Left Error.AllWarnings {} -> Hedgehog.assert (not (Language.PureScript.Errors.nonEmpty errors))
    Left Error.ErrorsAndWarnings {} -> Hedgehog.assert (Language.PureScript.Errors.nonEmpty errors)
    Right _ -> Hedgehog.failure

genError :: Hedgehog.Gen Error.Error
genError = do
  errors <- genMultipleErrors
  warnings <- genMultipleErrors
  Hedgehog.Gen.element
    [ Error.AllErrors {Error.errors},
      Error.AllWarnings {Error.warnings},
      Error.ErrorsAndWarnings {Error.errors, Error.warnings}
    ]

genMultipleErrors :: Hedgehog.Gen Language.PureScript.Errors.MultipleErrors
genMultipleErrors = do
  simpleErrorMessages <- Hedgehog.Gen.list (Hedgehog.Range.linear 0 3) genSimpleErrorMessage
  pure (foldMap Language.PureScript.Errors.errorMessage simpleErrorMessages)

genNonEmptyMultipleErrors :: Hedgehog.Gen Language.PureScript.Errors.MultipleErrors
genNonEmptyMultipleErrors = do
  simpleErrorMessage <- genSimpleErrorMessage
  pure (Language.PureScript.Errors.errorMessage simpleErrorMessage)

genSimpleErrorMessage :: Hedgehog.Gen Language.PureScript.Errors.SimpleErrorMessage
genSimpleErrorMessage =
  Hedgehog.Gen.element
    [ Language.PureScript.Errors.InvalidDoLet
    ]

showError ::
  Error.Error ->
  String
showError error = case error of
  Error.AllErrors {} -> "AllErrors"
  Error.AllWarnings {} -> "AllWarnings"
  Error.ErrorsAndWarnings {} -> "ErrorsAndWarnings"
