{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CompileModule
  ( Mode,
    modeParser,
    modeParserInfo,
    modeRun,
  )
where

import qualified "mtl" Control.Monad.Error.Class
import qualified "purescript" Control.Monad.Supply
import qualified "this" CoreFn
import qualified "containers" Data.Map
import qualified "this" Error
import qualified "this" ExternsFile
import qualified "this" JavaScript
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.Docs.Types
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Cache
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.Time

-- |
-- The different modes for `compile`.
data Mode = Mode
  { -- |
    -- Any CoreFn files we deal with.
    coreFnFiles :: Maybe CoreFn.Mode,
    -- |
    -- Any externs files we deal with.
    externsFiles :: Maybe ExternsFile.Mode,
    -- |
    -- Whether to opt-out of any warnings.
    ignoreWarnings :: Bool,
    -- |
    -- Any JavaScript files we deal with.
    javaScriptFiles :: Maybe JavaScript.Mode,
    -- |
    -- We need the actual PureScript file to compile.
    pureScriptFile :: FilePath
  }

instance Display Mode where
  display :: Mode -> Utf8Builder
  display compileMode = case compileMode of
    Mode {coreFnFiles = coreFnFiles', externsFiles = externsFiles', ignoreWarnings, javaScriptFiles = javaScriptFiles', pureScriptFile} ->
      "CompileModule.Mode { "
        <> foldMap
          ( \coreFnFiles ->
              "coreFnFiles = "
                <> display coreFnFiles
                <> ", "
          )
          coreFnFiles'
        <> foldMap
          ( \externsFiles ->
              "externsFiles = "
                <> display externsFiles
                <> ", "
          )
          externsFiles'
        <> "ignoreWarnings = "
        <> displayShow ignoreWarnings
        <> ", "
        <> foldMap
          ( \javaScriptFiles ->
              "javaScriptFiles = "
                <> display javaScriptFiles
                <> ", "
          )
          javaScriptFiles'
        <> "pureScriptFile = "
        <> displayShow pureScriptFile
        <> " }"

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  pure Mode
    <*> optional CoreFn.modeParser
    <*> optional ExternsFile.modeParser
    <*> ignoreWarnings
    <*> optional JavaScript.modeParser
    <*> pureScriptFile
  where
    ignoreWarnings :: Options.Applicative.Parser Bool
    ignoreWarnings =
      Options.Applicative.switch
        ( Options.Applicative.help "Don't let warnings fail compilation"
            <> Options.Applicative.long "ignore-warnings"
        )

    pureScriptFile :: Options.Applicative.Parser FilePath
    pureScriptFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "The PureScript source file to compile"
            <> Options.Applicative.long "purs-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- This wraps the @parser@ with some more information and helper text.
modeParserInfo :: Options.Applicative.ParserInfo Mode
modeParserInfo = Options.Applicative.info modeParser description
  where
    description :: Options.Applicative.InfoMod Mode
    description =
      Options.Applicative.progDesc "Compile a single module. This exists for symmetry with the other commands."

-- |
-- We handle the different alternates of @Mode@ and run each one appropriately.
modeRun ::
  Mode ->
  RIO SimpleApp (Either Error.Error Utf8Builder)
modeRun mode = case mode of
  Mode {coreFnFiles, ignoreWarnings, javaScriptFiles, pureScriptFile} -> do
    logDebugS "purs-compile-module" ("Processing compile mode: " <> display mode)
    externs' <- case externsFiles mode of
      Nothing -> pure (Right [])
      Just externsFiles -> ExternsFile.readAll externsFiles
    case externs' of
      Left error -> pure (Left error)
      Right externs -> do
        result <- liftIO do
          Language.PureScript.Make.runMake Language.PureScript.Options.defaultOptions do
            pureScriptFileContents <- Language.PureScript.Make.Monad.readTextFile pureScriptFile
            pureScriptModule <- case Language.PureScript.CST.parseFromFile pureScriptFile pureScriptFileContents of
              Left parseErrors -> Control.Monad.Error.Class.throwError (Language.PureScript.CST.toMultipleErrors pureScriptFile parseErrors)
              Right pureScriptModule -> pure pureScriptModule
            Language.PureScript.Make.rebuildModule (makeActions coreFnFiles (externsFiles mode) javaScriptFiles) externs pureScriptModule
        pure (Error.fromRebuildModule ignoreWarnings result)

-- |
-- Our set of @Language.PureScript.Make.Actions.MakeActions@ that work for a compiling a single module.
makeActions ::
  Maybe CoreFn.Mode ->
  Maybe ExternsFile.Mode ->
  Maybe JavaScript.Mode ->
  Language.PureScript.Make.Actions.MakeActions Language.PureScript.Make.Monad.Make
makeActions coreFn' externs' javaScript' =
  Language.PureScript.Make.Actions.MakeActions
    { Language.PureScript.Make.Actions.codegen,
      Language.PureScript.Make.Actions.ffiCodegen,
      Language.PureScript.Make.Actions.getInputTimestampsAndHashes,
      Language.PureScript.Make.Actions.getOutputTimestamp,
      Language.PureScript.Make.Actions.outputPrimDocs,
      Language.PureScript.Make.Actions.progress,
      Language.PureScript.Make.Actions.readCacheDb,
      Language.PureScript.Make.Actions.readExterns,
      Language.PureScript.Make.Actions.writeCacheDb
    }
  where
    codegen ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      Language.PureScript.Docs.Types.Module ->
      Language.PureScript.Externs.ExternsFile ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegen coreFnModule _ externsFile = do
      for_ coreFn' (CoreFn.codegen coreFnModule)
      for_ externs' (ExternsFile.codegen externsFile)
      for_ javaScript' (JavaScript.codegen coreFnModule)

    ffiCodegen ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      Language.PureScript.Make.Monad.Make ()
    ffiCodegen _ = pure ()

    getInputTimestampsAndHashes ::
      Language.PureScript.Names.ModuleName ->
      Language.PureScript.Make.Monad.Make (Either Language.PureScript.Make.Actions.RebuildPolicy (Map FilePath (RIO.Time.UTCTime, Language.PureScript.Make.Monad.Make Language.PureScript.Make.Cache.ContentHash)))
    getInputTimestampsAndHashes _ = pure (Left Language.PureScript.Make.Actions.RebuildAlways)

    getOutputTimestamp ::
      Language.PureScript.Names.ModuleName ->
      Language.PureScript.Make.Monad.Make (Maybe RIO.Time.UTCTime)
    getOutputTimestamp _ = pure Nothing

    outputPrimDocs :: Language.PureScript.Make.Monad.Make ()
    outputPrimDocs = pure ()

    progress ::
      Language.PureScript.Make.Actions.ProgressMessage ->
      Language.PureScript.Make.Monad.Make ()
    progress progressMessage = case progressMessage of
      Language.PureScript.Make.Actions.CompilingModule moduleName -> do
        liftIO (hPutBuilder stdout (getUtf8Builder ("Compiling " <> display (Language.PureScript.Names.runModuleName moduleName) <> newline)))

    readCacheDb :: Language.PureScript.Make.Monad.Make Language.PureScript.Make.Cache.CacheDb
    readCacheDb = pure Data.Map.empty

    readExterns ::
      Language.PureScript.Names.ModuleName ->
      Language.PureScript.Make.Monad.Make (FilePath, Maybe Language.PureScript.Externs.ExternsFile)
    readExterns _ = pure ("", Nothing)

    writeCacheDb ::
      Language.PureScript.Make.Cache.CacheDb ->
      Language.PureScript.Make.Monad.Make ()
    writeCacheDb _ = pure ()

-- |
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"
