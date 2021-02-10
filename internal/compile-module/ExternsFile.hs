{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExternsFile
  ( Mode,
    codegen,
    modeParser,
    readAll,
  )
where

import qualified Control.Monad.Except
import qualified "purescript" Control.Monad.Supply
import qualified "this" Error
import qualified "purescript" Language.PureScript.AST.SourcePos
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.ModuleDependencies
import qualified "purescript" Language.PureScript.Names
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "this" SignatureExternsFile

-- |
-- The data for compiling externs files.
data Mode = Mode
  { -- |
    -- The input externs files we depend on.
    inputs :: [FilePath],
    -- |
    -- The output externs file we expect to generate.
    -- This is a "standard" externs file with valid source annotations.
    -- A "standard" externs file should be able to be used by other PureScript tooling without issue.
    outputStandard :: Maybe FilePath,
    -- |
    -- The output externs file we expect to generate.
    -- This is a "signature" externs file where all source annotations are "null".
    -- Since information about the arrangement of the module is removed,
    -- this "signature" externs file can be depended on more reliably than a "standard" externs file.
    outputSignature :: Maybe FilePath
  }

instance Display Mode where
  display :: Mode -> Utf8Builder
  display externs = case externs of
    Mode {inputs, outputStandard, outputSignature} ->
      "ExternsFile.Mode { "
        <> "inputs = "
        <> displayShow inputs
        <> ", "
        <> "outputStandard = "
        <> displayShow outputStandard
        <> ", "
        <> "outputSignature = "
        <> displayShow outputSignature
        <> " }"

codegen ::
  Language.PureScript.Externs.ExternsFile ->
  Mode ->
  Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
codegen externsFile externs = do
  for_ (outputStandard externs) \outputExternsFile -> do
    lift (Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile)
  for_ (outputSignature externs) \outputExternsFile -> do
    lift (SignatureExternsFile.codegen externsFile outputExternsFile)

-- |
-- We have to convert to what the module signature needs for its imports so it we can sort the externs properly.
--
-- Much like with @externsModuleSignature@,
-- it seems like this is something that should happen uniformly so every command doesn't have to do it itself.
externsImportModuleSignatureImport ::
  Language.PureScript.Externs.ExternsImport ->
  (Language.PureScript.Names.ModuleName, Language.PureScript.AST.SourcePos.SourceSpan)
externsImportModuleSignatureImport externsImport =
  ( Language.PureScript.Externs.eiModule externsImport,
    Language.PureScript.AST.SourcePos.nullSourceSpan
  )

-- |
-- We have to convert to a @Language.PureScript.ModuleDependencies.ModuleSignature@ so we can sort the externs properly.
--
-- Each of the upstream commands has to do this same conversion for sorting the externs.
-- Seems a bit error prone to force each command to do the same exact sorting.
externsModuleSignature ::
  Language.PureScript.Externs.ExternsFile ->
  Language.PureScript.ModuleDependencies.ModuleSignature
externsModuleSignature externsFile =
  Language.PureScript.ModuleDependencies.ModuleSignature
    { Language.PureScript.ModuleDependencies.sigSourceSpan = Language.PureScript.Externs.efSourceSpan externsFile,
      Language.PureScript.ModuleDependencies.sigModuleName = Language.PureScript.Externs.efModuleName externsFile,
      Language.PureScript.ModuleDependencies.sigImports = fmap externsImportModuleSignatureImport (Language.PureScript.Externs.efImports externsFile)
    }

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  pure Mode
    <*> many input
    <*> optional outputStandard
    <*> optional outputSignature
  where
    input :: Options.Applicative.Parser FilePath
    input =
      Options.Applicative.strOption
        ( Options.Applicative.help "A pre-compiled externs file"
            <> Options.Applicative.long "input-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

    outputStandard :: Options.Applicative.Parser FilePath
    outputStandard =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the \"standard\" externs file. This externs file has valid source annotations"
            <> Options.Applicative.long "output-standard-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

    outputSignature :: Options.Applicative.Parser FilePath
    outputSignature =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the \"signature\" externs file. This externs file has \"null\" source annotations"
            <> Options.Applicative.long "output-signature-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- Read all of the input externs files.
readAll ::
  Mode ->
  RIO SimpleApp (Either Error.Error [Language.PureScript.Externs.ExternsFile])
readAll externsFiles = do
  Control.Monad.Except.runExceptT do
    Control.Monad.Except.withExceptT Error.AllErrors do
      logDebugS "purs-compile-module" ("Parsing externs files: " <> display externsFiles)
      unsortedExterns <- forMaybeA (inputs externsFiles) \inputExternsFile -> do
        Language.PureScript.Make.Monad.readExternsFile inputExternsFile
      logDebugS "purs-compile-module" ("Unsorted externs: " <> displayShow unsortedExterns)
      externsAndGraph <- Language.PureScript.ModuleDependencies.sortModules externsModuleSignature unsortedExterns
      logDebugS "purs-compile-module" ("Sorted externs and graph: " <> displayShow externsAndGraph)
      pure (fst externsAndGraph)
