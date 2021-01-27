{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified "mtl" Control.Monad.Error.Class
import qualified "purescript" Control.Monad.Supply
import qualified "containers" Data.Map
import qualified "base" Data.Version
import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.AST.Declarations
import qualified "purescript" Language.PureScript.AST.SourcePos
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CodeGen.JS
import qualified "purescript" Language.PureScript.CodeGen.JS.Printer
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.CoreFn.ToJSON
import qualified "purescript" Language.PureScript.CoreImp.AST
import qualified "purescript" Language.PureScript.Docs.Types
import qualified "purescript" Language.PureScript.Environment
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

-- |
-- The arguments to the program.
data Arguments = Arguments
  { -- |
    -- The actual @Mode@ we want to run.
    mode :: Mode,
    -- |
    -- Whether we want to display debug information or not.
    verbose :: Bool
  }

-- |
-- The different modes for `compile`.
data CompileMode = CompileMode
  { -- |
    -- Any CoreFn files we deal with.
    coreFnFiles :: Maybe CompileModeCoreFn,
    -- |
    -- Any externs files we deal with.
    externsFiles :: Maybe CompileModeExterns,
    -- |
    -- Whether to opt-out of any warnings.
    ignoreWarnings :: Bool,
    -- |
    -- Any JavaScript files we deal with.
    javaScriptFiles :: Maybe CompileModeJavaScript,
    -- |
    -- We need the actual PureScript file to compile.
    pureScriptFile :: FilePath
  }

instance Display CompileMode where
  display :: CompileMode -> Utf8Builder
  display compileMode = case compileMode of
    CompileMode {coreFnFiles = coreFnFiles', externsFiles = externsFiles', ignoreWarnings, javaScriptFiles = javaScriptFiles', pureScriptFile} ->
      "CompileMode { "
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
-- The data for compiling CoreFn files.
newtype CompileModeCoreFn = CompileModeCoreFn
  { -- |
    -- The output CoreFn file we expect to generate.
    outputCoreFnFile :: FilePath
  }

instance Display CompileModeCoreFn where
  display :: CompileModeCoreFn -> Utf8Builder
  display compileModeCoreFn = case compileModeCoreFn of
    CompileModeCoreFn {outputCoreFnFile} ->
      "CompileModeCoreFn { "
        <> "outputCoreFnFile = "
        <> displayShow outputCoreFnFile
        <> " }"

-- |
-- The data for compiling externs files.
data CompileModeExterns = CompileModeExterns
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

instance Display CompileModeExterns where
  display :: CompileModeExterns -> Utf8Builder
  display compileModeExterns = case compileModeExterns of
    CompileModeExterns {inputExternsFiles, outputStandardExternsFile, outputSignatureExternsFile} ->
      "CompileModeExterns { "
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
data CompileModeFFI = CompileModeFFI
  { -- |
    -- The input FFI file we depend on.
    inputFFIFile :: FilePath,
    -- |
    -- The output FFI file we expect to generate.
    outputFFIFile :: FilePath
  }

instance Display CompileModeFFI where
  display :: CompileModeFFI -> Utf8Builder
  display compileModeFFI = case compileModeFFI of
    CompileModeFFI {inputFFIFile, outputFFIFile} ->
      "CompileModeFFI { "
        <> "inputFFIFile = "
        <> displayShow inputFFIFile
        <> ", "
        <> "outputFFIFile = "
        <> displayShow outputFFIFile
        <> " }"

-- |
-- The data for compiling JavaScript files.
data CompileModeJavaScript = CompileModeJavaScript
  { -- |
    -- Any FFI files we deal with.
    ffiFiles :: Maybe CompileModeFFI,
    -- |
    -- The output JavaScript file we expect to generate.
    outputJavaScriptFile :: FilePath
  }

instance Display CompileModeJavaScript where
  display :: CompileModeJavaScript -> Utf8Builder
  display compileModeJavaScript = case compileModeJavaScript of
    CompileModeJavaScript {ffiFiles = ffiFiles', outputJavaScriptFile} ->
      "CompileModeJavaScript { "
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
-- The different modes we can run in.
data Mode
  = -- |
    -- Compile a module
    Compile CompileMode
  | -- |
    -- Show the version
    Version VersionMode

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
    Compile _ -> "Compile"
    Version _ -> "Version"

-- |
-- The different modes for `version`.
data VersionMode
  = -- |
    -- The "human" mode is intended to provide more information that humans might find useful.
    Human
  | -- |
    -- The "numeric" mode is intended for easier to parse information that a program might consume.
    Numeric

instance Display VersionMode where
  display :: VersionMode -> Utf8Builder
  display versionMode = case versionMode of
    Human -> "Human"
    Numeric -> "Numeric"

-- |
-- The actual parser for @Arguments@.
argumentsParser :: Options.Applicative.Parser Arguments
argumentsParser =
  pure Arguments
    <*> modeParser
    <*> verbose
  where
    verbose :: Options.Applicative.Parser Bool
    verbose =
      Options.Applicative.switch
        ( Options.Applicative.help "Display debug information to STDERR"
            <> Options.Applicative.long "verbose"
        )

-- |
-- This wraps the @argumentsParser@ with some more information and helper text.
argumentsParserInfo :: Options.Applicative.ParserInfo Arguments
argumentsParserInfo =
  Options.Applicative.info (Options.Applicative.helper <*> argumentsParser) description
  where
    description :: Options.Applicative.InfoMod Arguments
    description =
      Options.Applicative.fullDesc
        <> Options.Applicative.progDesc "Compile a single PureScript module"
        <> Options.Applicative.header "purs-compile-module - A PureScript compiler"

-- |
-- The actual parser for @CompileModeCoreFn@.
compileModeCoreFnParser :: Options.Applicative.Parser CompileModeCoreFn
compileModeCoreFnParser =
  pure CompileModeCoreFn
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
-- The actual parser for @CompileModeExterns@.
compileModeExternsParser :: Options.Applicative.Parser CompileModeExterns
compileModeExternsParser =
  pure CompileModeExterns
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
-- The actual parser for @CompileModeFFI@.
compileModeFFIParser :: Options.Applicative.Parser CompileModeFFI
compileModeFFIParser =
  pure CompileModeFFI
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
-- The actual parser for @CompileModeJavaScript@.
compileModeJavaScriptParser :: Options.Applicative.Parser CompileModeJavaScript
compileModeJavaScriptParser =
  pure CompileModeJavaScript
    <*> optional compileModeFFIParser
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
-- The actual parser for @CompileMode@.
compileModeParser :: Options.Applicative.Parser CompileMode
compileModeParser =
  pure CompileMode
    <*> optional compileModeCoreFnParser
    <*> optional compileModeExternsParser
    <*> ignoreWarnings
    <*> optional compileModeJavaScriptParser
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
-- This wraps the @compileModeParser@ with some more information and helper text.
compileModeParserInfo :: Options.Applicative.ParserInfo CompileMode
compileModeParserInfo = Options.Applicative.info compileModeParser description
  where
    description :: Options.Applicative.InfoMod CompileMode
    description =
      Options.Applicative.progDesc "Compile a single module. This exists for symmetry with the other commands."

-- |
-- We handle the different alternates of @CompileMode@ and run each one appropriately.
compileModeRun ::
  CompileMode ->
  RIO SimpleApp (Either Error Utf8Builder)
compileModeRun mode = case mode of
  CompileMode {coreFnFiles, externsFiles, ignoreWarnings, javaScriptFiles, pureScriptFile} -> do
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
    case result of
      (Left errors, warnings)
        | ignoreWarnings,
          Language.PureScript.Errors.nonEmpty errors,
          Language.PureScript.Errors.nonEmpty warnings -> do
          pure (Left (ErrorsAndWarnings {errors, warnings}))
        | ignoreWarnings,
          Language.PureScript.Errors.nonEmpty warnings -> do
          pure (Left (AllWarnings {warnings}))
        | Language.PureScript.Errors.nonEmpty warnings -> do
          pure (Left (AllErrors {errors = errors <> warnings}))
        | Language.PureScript.Errors.nonEmpty errors -> do
          pure (Left (AllErrors {errors = errors}))
        | otherwise -> do
          pure (Right "")
      (Right _, warnings)
        | ignoreWarnings,
          Language.PureScript.Errors.nonEmpty warnings -> do
          pure (Left (AllWarnings {warnings}))
        | Language.PureScript.Errors.nonEmpty warnings -> pure (Left (AllErrors {errors = warnings}))
        | otherwise -> pure (Right "")

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
-- Our entry point to `purs-compile-module`.
--
-- When run on a terminal,
-- this will program will parse arguments given to it and attempt to compile a single PureScript module.
-- There should be different modes that can be run based on the arguments given to it.
main :: IO ()
main = do
  arguments <- Options.Applicative.execParser argumentsParserInfo
  case arguments of
    Arguments {mode, verbose} -> do
      logOptions <- logOptionsHandle stderr verbose
      withLogFunc logOptions \logFunc -> do
        simpleApp <- mkSimpleApp logFunc Nothing
        runRIO simpleApp do
          logDebugS "purs-compile-module" ("Processing Mode: " <> display mode)
          result <- case mode of
            Compile compileMode -> compileModeRun compileMode
            Version versionMode -> versionModeRun versionMode
          case result of
            Left error -> case error of
              AllErrors {} -> do
                logErrorS "purs-compile-module" (display error)
                exitFailure
              AllWarnings {} -> do
                logWarnS "purs-compile-module" (display error)
                exitSuccess
              ErrorsAndWarnings {} -> do
                logErrorS "purs-compile-module" (display error)
                exitFailure
            Right output -> do
              hPutBuilder stdout (getUtf8Builder output)
              exitSuccess

-- |
-- Our set of @Language.PureScript.Make.Actions.MakeActions@ that work for a compiling a single module.
makeActions ::
  Maybe CompileModeCoreFn ->
  Maybe CompileModeExterns ->
  Maybe CompileModeJavaScript ->
  Language.PureScript.Make.Actions.MakeActions Language.PureScript.Make.Monad.Make
makeActions compileModeCoreFn' compileModeExterns' compileModeJavaScript' =
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
      for_ compileModeCoreFn' (codegenCoreFn coreFnModule)
      for_ compileModeExterns' (codegenExterns externsFile)
      for_ compileModeJavaScript' (codegenJavaScript coreFnModule)

    codegenCoreFn ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      CompileModeCoreFn ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenCoreFn coreFnModule compileModeCoreFn = do
      let coreFnJSON = Language.PureScript.CoreFn.ToJSON.moduleToJSON Language.PureScript.version coreFnModule
      lift (Language.PureScript.Make.Monad.writeJSONFile (outputCoreFnFile compileModeCoreFn) coreFnJSON)

    codegenExterns ::
      Language.PureScript.Externs.ExternsFile ->
      CompileModeExterns ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenExterns externsFile' compileModeExterns = do
      for_ (outputStandardExternsFile compileModeExterns) \outputExternsFile -> do
        lift (Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile')
      for_ (outputSignatureExternsFile compileModeExterns) \outputExternsFile -> do
        let externsFile :: Language.PureScript.Externs.ExternsFile
            externsFile = makeSignatureExternsFile externsFile'
        lift (Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile)

    codegenJavaScript ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      CompileModeJavaScript ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
    codegenJavaScript coreFnModule compileModeJavaScript = do
      ffiModule <- codegenFFI coreFnModule compileModeJavaScript
      coreImpASTs <- Language.PureScript.CodeGen.JS.moduleToJs coreFnModule ffiModule
      lift (Language.PureScript.Make.Monad.writeTextFile (outputJavaScriptFile compileModeJavaScript) (encodeUtf8 (Language.PureScript.CodeGen.JS.Printer.prettyPrintJS coreImpASTs)))

    codegenFFI ::
      Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
      CompileModeJavaScript ->
      Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make (Maybe Language.PureScript.CoreImp.AST.AST)
    codegenFFI coreFnModule compileModeJavaScript = do
      lift (validateFFIModule coreFnModule (ffiFiles compileModeJavaScript))
      for (ffiFiles compileModeJavaScript) \compileModeFFI -> do
        lift (Language.PureScript.Make.Monad.copyFile (inputFFIFile compileModeFFI) (outputFFIFile compileModeFFI))
        pure
          ( Language.PureScript.CoreImp.AST.App
              Nothing
              (Language.PureScript.CoreImp.AST.Var Nothing "require")
              [ Language.PureScript.CoreImp.AST.StringLiteral
                  Nothing
                  ( fromString
                      ( importRelativeTo
                          (outputFFIFile compileModeFFI)
                          (outputJavaScriptFile compileModeJavaScript)
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
      Maybe CompileModeFFI ->
      Language.PureScript.Make.Monad.Make ()
    validateFFIModule coreFnModule compileModeFFI' = do
      case (Language.PureScript.CoreFn.Module.moduleForeign coreFnModule, compileModeFFI') of
        ([], Just compileModeFFI) -> do
          Control.Monad.Error.Class.throwError
            ( Language.PureScript.Errors.errorMessage'
                (Language.PureScript.CoreFn.Module.moduleSourceSpan coreFnModule)
                ( Language.PureScript.Errors.UnnecessaryFFIModule
                    (Language.PureScript.CoreFn.Module.moduleName coreFnModule)
                    (inputFFIFile compileModeFFI)
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
        (_, Just compileModeFFI) -> do
          Language.PureScript.Make.Actions.checkForeignDecls coreFnModule (inputFFIFile compileModeFFI)

    writeCacheDb ::
      Language.PureScript.Make.Cache.CacheDb ->
      Language.PureScript.Make.Monad.Make ()
    writeCacheDb _ = pure ()

makeSignatureDeclarationRef ::
  Language.PureScript.AST.Declarations.DeclarationRef ->
  Language.PureScript.AST.Declarations.DeclarationRef
makeSignatureDeclarationRef declarationRef' = case declarationRef' of
  Language.PureScript.AST.Declarations.KindRef _ kindName ->
    Language.PureScript.AST.Declarations.KindRef Language.PureScript.AST.SourcePos.nullSourceSpan kindName
  Language.PureScript.AST.Declarations.ModuleRef _ moduleName ->
    Language.PureScript.AST.Declarations.ModuleRef Language.PureScript.AST.SourcePos.nullSourceSpan moduleName
  Language.PureScript.AST.Declarations.ReExportRef _ exportSource declarationRef ->
    Language.PureScript.AST.Declarations.ReExportRef Language.PureScript.AST.SourcePos.nullSourceSpan exportSource (makeSignatureDeclarationRef declarationRef)
  Language.PureScript.AST.Declarations.TypeClassRef _ className ->
    Language.PureScript.AST.Declarations.TypeClassRef Language.PureScript.AST.SourcePos.nullSourceSpan className
  Language.PureScript.AST.Declarations.TypeInstanceRef _ instanceName ->
    Language.PureScript.AST.Declarations.TypeInstanceRef Language.PureScript.AST.SourcePos.nullSourceSpan instanceName
  Language.PureScript.AST.Declarations.TypeOpRef _ opName ->
    Language.PureScript.AST.Declarations.TypeOpRef Language.PureScript.AST.SourcePos.nullSourceSpan opName
  Language.PureScript.AST.Declarations.TypeRef _ typeName constructorNames ->
    Language.PureScript.AST.Declarations.TypeRef Language.PureScript.AST.SourcePos.nullSourceSpan typeName constructorNames
  Language.PureScript.AST.Declarations.ValueOpRef _ opName ->
    Language.PureScript.AST.Declarations.ValueOpRef Language.PureScript.AST.SourcePos.nullSourceSpan opName
  Language.PureScript.AST.Declarations.ValueRef _ ident ->
    Language.PureScript.AST.Declarations.ValueRef Language.PureScript.AST.SourcePos.nullSourceSpan ident

makeSignatureExternsDeclaration ::
  Language.PureScript.Externs.ExternsDeclaration ->
  Language.PureScript.Externs.ExternsDeclaration
makeSignatureExternsDeclaration externsDeclaration = case externsDeclaration of
  Language.PureScript.Externs.EDClass name typeArguments members constraints fundeps isEmpty ->
    Language.PureScript.Externs.EDClass
      name
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) typeArguments)
      ((fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) members)
      (fmap (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constraints)
      fundeps
      isEmpty
  Language.PureScript.Externs.EDDataConstructor name origin typeConstructor type' fields ->
    Language.PureScript.Externs.EDDataConstructor
      name
      origin
      typeConstructor
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')
      fields
  Language.PureScript.Externs.EDInstance className name types constraints chain chainIndex ->
    Language.PureScript.Externs.EDInstance
      className
      name
      (fmap (Language.PureScript.AST.SourcePos.nullSourceAnn <$) types)
      ((fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constraints)
      chain
      chainIndex
  Language.PureScript.Externs.EDKind name ->
    Language.PureScript.Externs.EDKind name
  Language.PureScript.Externs.EDType name kind declarationKind ->
    Language.PureScript.Externs.EDType
      name
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ kind)
      (makeSignatureTypeKind declarationKind)
  Language.PureScript.Externs.EDTypeSynonym name arguments type' ->
    Language.PureScript.Externs.EDTypeSynonym
      name
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) arguments)
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')
  Language.PureScript.Externs.EDValue name type' ->
    Language.PureScript.Externs.EDValue
      name
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')

makeSignatureExternsFile ::
  Language.PureScript.Externs.ExternsFile ->
  Language.PureScript.Externs.ExternsFile
makeSignatureExternsFile externsFile = case externsFile of
  Language.PureScript.Externs.ExternsFile version' moduleName exports imports fixities typeFixities declarations _sourceSpan ->
    Language.PureScript.Externs.ExternsFile
      version'
      moduleName
      (fmap makeSignatureDeclarationRef exports)
      (fmap makeSignatureExternsImport imports)
      fixities
      typeFixities
      (fmap makeSignatureExternsDeclaration declarations)
      Language.PureScript.AST.SourcePos.nullSourceSpan

makeSignatureExternsImport ::
  Language.PureScript.Externs.ExternsImport ->
  Language.PureScript.Externs.ExternsImport
makeSignatureExternsImport externsImport = case externsImport of
  Language.PureScript.Externs.ExternsImport moduleName importDeclarationType importedAs ->
    Language.PureScript.Externs.ExternsImport
      moduleName
      (makeSignatureImportDeclarationType importDeclarationType)
      importedAs

makeSignatureImportDeclarationType ::
  Language.PureScript.AST.Declarations.ImportDeclarationType ->
  Language.PureScript.AST.Declarations.ImportDeclarationType
makeSignatureImportDeclarationType importDeclarationType = case importDeclarationType of
  Language.PureScript.AST.Declarations.Explicit imports ->
    Language.PureScript.AST.Declarations.Explicit
      (fmap makeSignatureDeclarationRef imports)
  Language.PureScript.AST.Declarations.Hiding imports ->
    Language.PureScript.AST.Declarations.Hiding
      (fmap makeSignatureDeclarationRef imports)
  Language.PureScript.AST.Declarations.Implicit ->
    Language.PureScript.AST.Declarations.Implicit

makeSignatureTypeKind ::
  Language.PureScript.Environment.TypeKind ->
  Language.PureScript.Environment.TypeKind
makeSignatureTypeKind typeKind = case typeKind of
  Language.PureScript.Environment.DataType typeArguments constructors ->
    Language.PureScript.Environment.DataType
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) typeArguments)
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constructors)
  Language.PureScript.Environment.TypeSynonym -> Language.PureScript.Environment.TypeSynonym
  Language.PureScript.Environment.ExternData -> Language.PureScript.Environment.ExternData
  Language.PureScript.Environment.LocalTypeVariable -> Language.PureScript.Environment.LocalTypeVariable
  Language.PureScript.Environment.ScopedTypeVar -> Language.PureScript.Environment.ScopedTypeVar

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  asum
    [ Options.Applicative.hsubparser
        ( fold
            [ Options.Applicative.command "compile" (fmap Compile compileModeParserInfo),
              Options.Applicative.command "version" (fmap Version versionModeParserInfo)
            ]
        ),
      fmap Compile compileModeParser
    ]

-- |
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"

-- |
-- The version of `purs-compile-module`.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 0, 0]

-- |
-- The actual parser for @VersionMode@.
versionModeParser :: Options.Applicative.Parser VersionMode
versionModeParser =
  asum
    [ pure Human,
      Options.Applicative.flag' Numeric versionNumeric
    ]
  where
    versionNumeric :: Options.Applicative.Mod Options.Applicative.FlagFields a
    versionNumeric =
      Options.Applicative.help "Print machine-readable version number only"
        <> Options.Applicative.long "numeric"

-- |
-- This wraps the @versionModeParser@ with some more information and helper text.
versionModeParserInfo :: Options.Applicative.ParserInfo VersionMode
versionModeParserInfo = Options.Applicative.info versionModeParser description
  where
    description :: Options.Applicative.InfoMod VersionMode
    description =
      Options.Applicative.progDesc "Print version information"

-- |
-- We handle the different alternates of @VersionMode@ and run each one appropriately.
versionModeRun ::
  VersionMode ->
  RIO SimpleApp (Either Error Utf8Builder)
versionModeRun mode = do
  logDebugS "purs-compile-module" ("Processing version mode: " <> display mode)
  case mode of
    Human -> versionModeRunHuman
    Numeric -> versionModeRunNumeric

-- |
-- We want to provide a few versions for humans:
-- - The version of our `purs-compile-module` program.
-- - The version of the underlying `purs` program.
--
-- This is because people generally need this information when they're diagnosing issues.
versionModeRunHuman :: RIO SimpleApp (Either Error Utf8Builder)
versionModeRunHuman = do
  logDebugS "purs-compile-module" "Rendering human-readable version"
  pure
    ( Right
        ( "purs-compile-module: "
            <> fromString (Data.Version.showVersion version)
            <> newline
            <> "purs: "
            <> fromString (Data.Version.showVersion Language.PureScript.version)
            <> newline
        )
    )

-- |
-- We want to only provide the version of our `purs-compile-module` program.
-- The intent behind providing a simple version is that some other program can easily parse the version if it's plain.
versionModeRunNumeric :: RIO SimpleApp (Either Error Utf8Builder)
versionModeRunNumeric = do
  logDebugS "purs-compile-module" "Rendering numeric version"
  pure
    ( Right
        ( fromString (Data.Version.showVersion version)
            <> newline
        )
    )
