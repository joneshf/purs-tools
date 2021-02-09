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
import qualified "containers" Data.Map
import qualified "this" Error
import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.AST.SourcePos
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CodeGen.JS
import qualified "purescript" Language.PureScript.CodeGen.JS.Printer
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.CoreFn.ToJSON
import qualified "purescript" Language.PureScript.CoreImp.AST
import qualified "purescript" Language.PureScript.Docs.Types
import qualified "purescript" Language.PureScript.Errors
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Cache
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.ModuleDependencies
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.FilePath
import qualified "rio" RIO.Time
import qualified "this" SignatureExternsFile

-- |
-- The data for compiling CoreFn files.
newtype CoreFnFiles = CoreFnFiles
  { -- |
    -- The output CoreFn file we expect to generate.
    outputCoreFnFile :: FilePath
  }

instance Display CoreFnFiles where
  display :: CoreFnFiles -> Utf8Builder
  display coreFn = case coreFn of
    CoreFnFiles {outputCoreFnFile} ->
      "CoreFnFiles { "
        <> "outputCoreFnFile = "
        <> displayShow outputCoreFnFile
        <> " }"

-- |
-- The data for compiling externs files.
data ExternsFiles = ExternsFiles
  { -- |
    -- The input externs files we depend on.
    inputExternsFiles :: [FilePath],
    -- |
    -- The output externs file we expect to generate.
    -- This is a "standard" externs file with valid source annotations.
    -- A "standard" externs file should be able to be used by other PureScript tooling without issue.
    outputStandardExternsFile :: Maybe FilePath,
    -- |
    -- The output externs file we expect to generate.
    -- This is a "signature" externs file where all source annotations are "null".
    -- Since information about the arrangement of the module is removed,
    -- this "signature" externs file can be depended on more reliably than a "standard" externs file.
    outputSignatureExternsFile :: Maybe FilePath
  }

instance Display ExternsFiles where
  display :: ExternsFiles -> Utf8Builder
  display externs = case externs of
    ExternsFiles {inputExternsFiles, outputStandardExternsFile, outputSignatureExternsFile} ->
      "ExternsFiles { "
        <> "inputExternsFiles = "
        <> displayShow inputExternsFiles
        <> ", "
        <> "outputStandardExternsFile = "
        <> displayShow outputStandardExternsFile
        <> ", "
        <> "outputSignatureExternsFile = "
        <> displayShow outputSignatureExternsFile
        <> " }"

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
    coreFnFiles :: Maybe CoreFnFiles,
    -- |
    -- Any externs files we deal with.
    externsFiles :: Maybe ExternsFiles,
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
-- The actual parser for @CoreFnFiles@.
coreFnFilesParser :: Options.Applicative.Parser CoreFnFiles
coreFnFilesParser =
  pure CoreFnFiles
    <*> outputCoreFnFile
  where
    outputCoreFnFile :: Options.Applicative.Parser FilePath
    outputCoreFnFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the compiled CoreFn file"
            <> Options.Applicative.long "output-corefn-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- The actual parser for @ExternsFiles@.
externsFilesParser :: Options.Applicative.Parser ExternsFiles
externsFilesParser =
  pure ExternsFiles
    <*> many inputExternsFile
    <*> optional outputStandardExternsFile
    <*> optional outputSignatureExternsFile
  where
    inputExternsFile :: Options.Applicative.Parser FilePath
    inputExternsFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "A pre-compiled externs file"
            <> Options.Applicative.long "input-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

    outputStandardExternsFile :: Options.Applicative.Parser FilePath
    outputStandardExternsFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the \"standard\" externs file. This externs file has valid source annotations"
            <> Options.Applicative.long "output-standard-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

    outputSignatureExternsFile :: Options.Applicative.Parser FilePath
    outputSignatureExternsFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the \"signature\" externs file. This externs file has \"null\" source annotations"
            <> Options.Applicative.long "output-signature-externs-file"
            <> Options.Applicative.metavar "FILE"
        )

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
    <*> optional coreFnFilesParser
    <*> optional externsFilesParser
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
  Mode {coreFnFiles, externsFiles, ignoreWarnings, javaScriptFiles, pureScriptFile} -> do
    logDebugS "purs-compile-module" ("Processing compile mode: " <> display mode)
    result <- liftIO do
      Language.PureScript.Make.runMake Language.PureScript.Options.defaultOptions do
        unsortedExterns <- forMaybeA (foldMap inputExternsFiles externsFiles) \inputExternsFile -> do
          Language.PureScript.Make.Monad.readExternsFile inputExternsFile
        externsAndGraph <- Language.PureScript.ModuleDependencies.sortModules externsModuleSignature unsortedExterns
        let externs :: [Language.PureScript.Externs.ExternsFile]
            externs = fst externsAndGraph
        pureScriptFileContents <- Language.PureScript.Make.Monad.readTextFile pureScriptFile
        pureScriptModule <- case Language.PureScript.CST.parseFromFile pureScriptFile pureScriptFileContents of
          Left parseErrors -> Control.Monad.Error.Class.throwError (Language.PureScript.CST.toMultipleErrors pureScriptFile parseErrors)
          Right pureScriptModule -> pure pureScriptModule
        Language.PureScript.Make.rebuildModule (makeActions coreFnFiles externsFiles javaScriptFiles) externs pureScriptModule
    pure (Error.fromRebuildModule ignoreWarnings result)

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
  Maybe CoreFnFiles ->
  Maybe ExternsFiles ->
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
      for_ coreFn' (codegenCoreFn coreFnModule)
      for_ externs' (codegenExterns externsFile)
      for_ javaScript' (codegenJavaScript coreFnModule)

    codegenCoreFn ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      CoreFnFiles ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenCoreFn coreFnModule coreFn = do
      let coreFnJSON = Language.PureScript.CoreFn.ToJSON.moduleToJSON Language.PureScript.version coreFnModule
      lift (Language.PureScript.Make.Monad.writeJSONFile (outputCoreFnFile coreFn) coreFnJSON)

    codegenExterns ::
      Language.PureScript.Externs.ExternsFile ->
      ExternsFiles ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenExterns externsFile externs = do
      for_ (outputStandardExternsFile externs) \outputExternsFile -> do
        lift (Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile)
      for_ (outputSignatureExternsFile externs) \outputExternsFile -> do
        lift (SignatureExternsFile.codegen externsFile outputExternsFile)

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
