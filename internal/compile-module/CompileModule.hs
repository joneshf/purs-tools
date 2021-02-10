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
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CodeGen.JS
import qualified "purescript" Language.PureScript.CodeGen.JS.Printer
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.CoreImp.AST
import qualified "purescript" Language.PureScript.Docs.Types
import qualified "purescript" Language.PureScript.Errors
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Cache
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.FilePath
import qualified "rio" RIO.Time

-- |
-- The data for compiling FFI files.
data FFIFiles = FFIFiles
  { -- |
    -- The input FFI file we depend on.
    inputFFIFile :: FilePath,
    -- |
    -- The output FFI file we expect to generate.
    outputFFIFile :: FilePath
  }

instance Display FFIFiles where
  display :: FFIFiles -> Utf8Builder
  display ffi = case ffi of
    FFIFiles {inputFFIFile, outputFFIFile} ->
      "FFIFiles { "
        <> "inputFFIFile = "
        <> displayShow inputFFIFile
        <> ", "
        <> "outputFFIFile = "
        <> displayShow outputFFIFile
        <> " }"

-- |
-- The data for compiling JavaScript files.
data JavaScriptFiles = JavaScriptFiles
  { -- |
    -- Any FFI files we deal with.
    ffiFiles :: Maybe FFIFiles,
    -- |
    -- The output JavaScript file we expect to generate.
    outputJavaScriptFile :: FilePath
  }

instance Display JavaScriptFiles where
  display :: JavaScriptFiles -> Utf8Builder
  display javaScript = case javaScript of
    JavaScriptFiles {ffiFiles = ffiFiles', outputJavaScriptFile} ->
      "JavaScriptFiles { "
        <> foldMap
          ( \ffiFiles ->
              "javaScriptFiles = "
                <> display ffiFiles
                <> ", "
          )
          ffiFiles'
        <> "outputJavaScriptFile = "
        <> displayShow outputJavaScriptFile
        <> " }"

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
    javaScriptFiles :: Maybe JavaScriptFiles,
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
-- The actual parser for @FFIFiles@.
ffiFilesParser :: Options.Applicative.Parser FFIFiles
ffiFilesParser =
  pure FFIFiles
    <*> inputFFIFile
    <*> outputFFIFile
  where
    inputFFIFile :: Options.Applicative.Parser FilePath
    inputFFIFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "A pre-compiled FFI file"
            <> Options.Applicative.long "input-ffi-file"
            <> Options.Applicative.metavar "FILE"
        )

    outputFFIFile :: Options.Applicative.Parser FilePath
    outputFFIFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the compiled FFI file"
            <> Options.Applicative.long "output-ffi-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- The actual parser for @JavaScriptFiles@.
javaScriptFilesParser :: Options.Applicative.Parser JavaScriptFiles
javaScriptFilesParser =
  pure JavaScriptFiles
    <*> optional ffiFilesParser
    <*> outputJavaScriptFile
  where
    outputJavaScriptFile :: Options.Applicative.Parser FilePath
    outputJavaScriptFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the compiled JavaScript file"
            <> Options.Applicative.long "output-javascript-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  pure Mode
    <*> optional CoreFn.modeParser
    <*> optional ExternsFile.modeParser
    <*> ignoreWarnings
    <*> optional javaScriptFilesParser
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
-- Constructs a common.js import where the first @FilePath@ is relative to the second.
--
-- There are likely bugs in this implementation.
--
-- >>> importRelativeTo "ffi.js" "index.js"
-- "./ffi.js"
-- >>> importRelativeTo "ffi.js" "./index.js"
-- "./ffi.js"
-- >>> importRelativeTo "./ffi.js" "index.js"
-- "./ffi.js"
-- >>> importRelativeTo "./ffi.js" "./index.js"
-- "./ffi.js"
-- >>> importRelativeTo "foo/bar/baz/ffi.js" "foo/index.js"
-- "./bar/baz/ffi.js"
-- >>> importRelativeTo "foo/bar/baz/ffi.js" "foo/qux/cor/gar/index.js"
-- "./../../../bar/baz/ffi.js"
-- >>> importRelativeTo "bar/baz/ffi.js" "foo/qux/cor/gar/index.js"
-- "./../../../../bar/baz/ffi.js"
-- >>> importRelativeTo "/foo/bar/baz/ffi.js" "foo/qux/cor/gar/index.js"
-- "/foo/bar/baz/ffi.js"
importRelativeTo ::
  FilePath ->
  FilePath ->
  FilePath
importRelativeTo ffi' javaScript'
  | RIO.FilePath.isAbsolute ffi' = ffi'
  | otherwise = case relativeDirectory of
    "" -> "./" <> ffiFileName
    _ -> "." <> relativeDirectory <> "/" <> ffiFileName
  where
    addDotDot :: [FilePath] -> [FilePath] -> [FilePath] -> [FilePath]
    addDotDot acc ffis' javaScripts' = case (ffis', javaScripts') of
      (ffis, []) -> acc <> ffis
      ([], _) -> acc
      (ffi : ffis, javaScript : javaScripts)
        | ffi == javaScript -> addDotDot acc ffis javaScripts
        | otherwise -> addDotDot (".." : acc) ffis' javaScripts

    -- Since we're working with a common.js import,
    -- we don't want to rely on the platform's separator.
    -- If we did,
    -- we might generate something like `require("./foo\\bar\\baz.js")`.
    -- That would work on Windows and fail on a Unix platform.
    --
    -- We want to use `"/"` explicitly as the separator so the path will work on both Windows and Unix platforms.
    combine :: FilePath -> FilePath -> FilePath
    combine directory fileName = directory <> "/" <> fileName

    ffiDirectories :: [FilePath]
    ffiDirectories = RIO.FilePath.splitDirectories ffiDirectory

    ffiDirectory :: FilePath
    ffiDirectory = RIO.FilePath.takeDirectory ffi'

    ffiFileName :: FilePath
    ffiFileName = RIO.FilePath.takeFileName ffi'

    javaScriptDirectories :: [FilePath]
    javaScriptDirectories = RIO.FilePath.splitDirectories javaScriptDirectory

    javaScriptDirectory :: FilePath
    javaScriptDirectory = RIO.FilePath.takeDirectory javaScript'

    relativeDirectories :: [FilePath]
    relativeDirectories = addDotDot [] ffiDirectories javaScriptDirectories

    relativeDirectory :: FilePath
    relativeDirectory = foldl' combine "" relativeDirectories

-- |
-- Our set of @Language.PureScript.Make.Actions.MakeActions@ that work for a compiling a single module.
makeActions ::
  Maybe CoreFn.Mode ->
  Maybe ExternsFile.Mode ->
  Maybe JavaScriptFiles ->
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
      for_ javaScript' (codegenJavaScript coreFnModule)

    codegenJavaScript ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      JavaScriptFiles ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenJavaScript coreFnModule javaScript = do
      ffiModule <- codegenFFI coreFnModule javaScript
      coreImpASTs <- Language.PureScript.CodeGen.JS.moduleToJs coreFnModule ffiModule
      lift (Language.PureScript.Make.Monad.writeTextFile (outputJavaScriptFile javaScript) (encodeUtf8 (Language.PureScript.CodeGen.JS.Printer.prettyPrintJS coreImpASTs)))

    codegenFFI ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      JavaScriptFiles ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make (Maybe Language.PureScript.CoreImp.AST.AST)
    codegenFFI coreFnModule javaScript = do
      lift (validateFFIModule coreFnModule (ffiFiles javaScript))
      for (ffiFiles javaScript) \ffi -> do
        lift (Language.PureScript.Make.Monad.copyFile (inputFFIFile ffi) (outputFFIFile ffi))
        pure
          ( Language.PureScript.CoreImp.AST.App
              Nothing
              (Language.PureScript.CoreImp.AST.Var Nothing "require")
              [ Language.PureScript.CoreImp.AST.StringLiteral
                  Nothing
                  ( fromString
                      ( importRelativeTo
                          (outputFFIFile ffi)
                          (outputJavaScriptFile javaScript)
                      )
                  )
              ]
          )

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

    validateFFIModule ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      Maybe FFIFiles ->
      Language.PureScript.Make.Monad.Make ()
    validateFFIModule coreFnModule ffi' = do
      case (Language.PureScript.CoreFn.Module.moduleForeign coreFnModule, ffi') of
        ([], Just ffi) -> do
          Control.Monad.Error.Class.throwError
            ( Language.PureScript.Errors.errorMessage'
                (Language.PureScript.CoreFn.Module.moduleSourceSpan coreFnModule)
                ( Language.PureScript.Errors.UnnecessaryFFIModule
                    (Language.PureScript.CoreFn.Module.moduleName coreFnModule)
                    (inputFFIFile ffi)
                )
            )
        ([], Nothing) -> pure ()
        (_, Nothing) -> do
          Control.Monad.Error.Class.throwError
            ( Language.PureScript.Errors.errorMessage'
                (Language.PureScript.CoreFn.Module.moduleSourceSpan coreFnModule)
                ( Language.PureScript.Errors.MissingFFIModule
                    (Language.PureScript.CoreFn.Module.moduleName coreFnModule)
                )
            )
        (_, Just ffi) -> do
          Language.PureScript.Make.Actions.checkForeignDecls coreFnModule (inputFFIFile ffi)

    writeCacheDb ::
      Language.PureScript.Make.Cache.CacheDb ->
      Language.PureScript.Make.Monad.Make ()
    writeCacheDb _ = pure ()

-- |
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"
