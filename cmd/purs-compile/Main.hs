{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified "aeson" Data.Aeson
import qualified "purescript" Language.PureScript.AST.Declarations
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Parser
import qualified "purescript" Language.PureScript.Errors
import qualified "purescript" Language.PureScript.Errors.JSON
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.ByteString.Lazy
import qualified "rio" RIO.Directory
import qualified "rio" RIO.List
import qualified "rio" RIO.Map
import qualified "rio" RIO.Set
import qualified "rio" RIO.Text
import qualified "ansi-terminal" System.Console.ANSI
import qualified "Glob" System.FilePath.Glob
import qualified "purescript" System.IO.UTF8

data CompileOptions = CompileOptions
  { pursFiles :: [FilePath],
    outputDirectory :: FilePath,
    options :: Language.PureScript.Options.Options,
    usePrefix :: Bool,
    jsonErrors :: Bool
  }

codegenTargetsMessage :: String
codegenTargetsMessage =
  "Accepted codegen targets are '"
    <> RIO.List.intercalate "', '" (RIO.Map.keys Language.PureScript.Options.codegenTargets)
    <> "'."

codegenTargetsParser :: Options.Applicative.Parser [Language.PureScript.Options.CodegenTarget]
codegenTargetsParser =
  Options.Applicative.option
    codegenTargetsReader
    ( Options.Applicative.short 'g'
        <> Options.Applicative.long "codegen"
        <> Options.Applicative.value [Language.PureScript.Options.JS]
        <> Options.Applicative.help
          ( "Specifies comma-separated codegen targets to include. "
              <> codegenTargetsMessage
              <> " The default target is 'js', but if this option is used only the targets specified will be used."
          )
    )

codegenTargetsReader :: Options.Applicative.ReadM [Language.PureScript.Options.CodegenTarget]
codegenTargetsReader = do
  targets <- Options.Applicative.str
  for (RIO.Text.split (== ',') targets) \targetText -> do
    let targetString = RIO.Text.unpack (RIO.Text.strip targetText)
    case RIO.Map.lookup targetString Language.PureScript.Options.codegenTargets of
      Just target -> pure target
      Nothing -> Options.Applicative.readerError codegenTargetsMessage

commentsParser :: Options.Applicative.Parser Bool
commentsParser =
  Options.Applicative.switch
    ( Options.Applicative.short 'c'
        <> Options.Applicative.long "comments"
        <> Options.Applicative.help "Include comments in the generated code"
    )

compile ::
  CompileOptions ->
  RIO SimpleApp ()
compile compileOptions = do
  input <- globWarningOnMisses (unless (jsonErrors compileOptions) . warnFileTypeNotFound) (pursFiles compileOptions)
  when (null input && not (jsonErrors compileOptions)) failNoPursFiles
  moduleFiles <- liftIO (System.IO.UTF8.readUTF8FilesT input)
  (makeErrors, makeWarnings) <- liftIO do
    Language.PureScript.Make.Monad.runMake (options compileOptions) do
      ms <- Language.PureScript.CST.parseModulesFromFiles id moduleFiles
      let filePathMap = RIO.Map.fromList (fmap filePathPair ms)
      foreigns <- Language.PureScript.Make.inferForeignModules filePathMap
      let makeActions = Language.PureScript.Make.Actions.buildMakeActions (outputDirectory compileOptions) filePathMap foreigns (usePrefix compileOptions)
      Language.PureScript.Make.make makeActions (fmap snd ms)
  printWarningsAndErrors (Language.PureScript.Options.optionsVerboseErrors (options compileOptions)) (jsonErrors compileOptions) makeWarnings makeErrors
  exitSuccess

compileOptionsDescription :: Options.Applicative.InfoMod CompileOptions
compileOptionsDescription =
  Options.Applicative.fullDesc
    <> Options.Applicative.progDesc "Compile PureScript source files"
    <> Options.Applicative.header "purs-compile - A PureScript compiler"

compileOptionsParser :: Options.Applicative.Parser CompileOptions
compileOptionsParser =
  pure CompileOptions
    <*> many pursFileParser
    <*> outputDirectoryParser
    <*> optionsParser
    <*> fmap not noPrefixParser
    <*> jsonErrorsParser

compileOptionsParserInfo :: Options.Applicative.ParserInfo CompileOptions
compileOptionsParserInfo =
  Options.Applicative.info
    (Options.Applicative.helper <*> compileOptionsParser)
    compileOptionsDescription

failNoPursFiles :: RIO SimpleApp ()
failNoPursFiles = do
  hPutBuilder
    stderr
    ( fromString
        ( unlines
            [ "purs-compile: No input files.",
              "Usage: For basic information, try the `--help' option."
            ]
        )
    )
  exitFailure

filePathPair ::
  forall a.
  (FilePath, Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module) ->
  (Language.PureScript.Names.ModuleName, Either a FilePath)
filePathPair (filePath, module') =
  ( Language.PureScript.AST.Declarations.getModuleName (Language.PureScript.CST.Parser.resPartial module'),
    Right filePath
  )

globWarningOnMisses ::
  (String -> RIO SimpleApp ()) ->
  [String] ->
  RIO SimpleApp [FilePath]
globWarningOnMisses warn globs = do
  filePathss <- traverse (globWithWarning warn) globs
  pure (concat filePathss)

globWithWarning ::
  (String -> RIO SimpleApp ()) ->
  String ->
  RIO SimpleApp [FilePath]
globWithWarning warn pattern' = do
  paths <- liftIO (System.FilePath.Glob.glob pattern')
  when (null paths) do
    warn pattern'
  pure paths

-- Ensure that the JS target is included if sourcemaps are
handleCodegenTargets ::
  [Language.PureScript.Options.CodegenTarget] ->
  RIO.Set.Set Language.PureScript.Options.CodegenTarget
handleCodegenTargets targets
  | Language.PureScript.Options.JSSourceMap `elem` targets = RIO.Set.fromList (Language.PureScript.Options.JS : targets)
  | otherwise = RIO.Set.fromList targets

jsonErrorsParser :: Options.Applicative.Parser Bool
jsonErrorsParser =
  Options.Applicative.switch
    ( Options.Applicative.long "json-errors"
        <> Options.Applicative.help "Print errors to stderr as JSON"
    )

jsonResult ::
  forall a.
  Bool ->
  Language.PureScript.Errors.MultipleErrors ->
  Either Language.PureScript.Errors.MultipleErrors a ->
  Language.PureScript.Errors.JSON.JSONResult
jsonResult verbose warnings errors' =
  Language.PureScript.Errors.JSON.JSONResult
    (Language.PureScript.Errors.JSON.toJSONErrors verbose Language.PureScript.Errors.Warning warnings)
    errors
  where
    errors :: [Language.PureScript.Errors.JSON.JSONError]
    errors = case errors' of
      Left x -> Language.PureScript.Errors.JSON.toJSONErrors verbose Language.PureScript.Errors.Error x
      Right _ -> []

-- |
-- Our entry point to `purs-compile`.
--
-- When run on a terminal,
-- this will program will parse arguments given to it and attempt to compile multiple PureScript modules.
main :: IO ()
main = do
  compileOptions <- Options.Applicative.execParser compileOptionsParserInfo
  logOptions <- logOptionsHandle stderr False
  withLogFunc logOptions \logFunc -> do
    simpleApp <- mkSimpleApp logFunc Nothing
    runRIO simpleApp do
      compile compileOptions

noPrefixParser :: Options.Applicative.Parser Bool
noPrefixParser =
  Options.Applicative.switch
    ( Options.Applicative.short 'p'
        <> Options.Applicative.long "no-prefix"
        <> Options.Applicative.help "Do not include comment header"
    )

optionsParser :: Options.Applicative.Parser Language.PureScript.Options.Options
optionsParser =
  pure Language.PureScript.Options.Options
    <*> verboseErrorsParser
    <*> fmap not commentsParser
    <*> fmap handleCodegenTargets codegenTargetsParser

outputDirectoryParser :: Options.Applicative.Parser FilePath
outputDirectoryParser =
  Options.Applicative.strOption
    ( Options.Applicative.short 'o'
        <> Options.Applicative.long "output"
        <> Options.Applicative.value "output"
        <> Options.Applicative.showDefault
        <> Options.Applicative.help "The output directory"
    )

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors ::
  forall a.
  Bool ->
  Bool ->
  Language.PureScript.Errors.MultipleErrors ->
  Either Language.PureScript.Errors.MultipleErrors a ->
  RIO SimpleApp ()
printWarningsAndErrors verbose useJSON warnings errors'
  | useJSON = do
    RIO.ByteString.Lazy.hPut stderr (Data.Aeson.encode (jsonResult verbose warnings errors'))
    case errors' of
      Left _ -> exitFailure
      Right _ -> pure ()
  | otherwise = do
    relativeDirectory <- RIO.Directory.getCurrentDirectory
    stderrSupportsColor <- liftIO (System.Console.ANSI.hSupportsANSI stderr)
    let colorCode =
          if stderrSupportsColor
            then Just Language.PureScript.Errors.defaultCodeColor
            else Nothing
    let ppeOptions =
          Language.PureScript.Errors.defaultPPEOptions
            { Language.PureScript.Errors.ppeCodeColor = colorCode,
              Language.PureScript.Errors.ppeFull = verbose,
              Language.PureScript.Errors.ppeRelativeDirectory = relativeDirectory
            }
    when (Language.PureScript.Errors.nonEmpty warnings) do
      hPutBuilder stderr (fromString (Language.PureScript.Errors.prettyPrintMultipleWarnings ppeOptions warnings))
    case errors' of
      Left errors -> do
        hPutBuilder stderr (fromString (Language.PureScript.Errors.prettyPrintMultipleErrors ppeOptions errors))
        exitFailure
      Right _ -> do
        pure ()

pursFileParser :: Options.Applicative.Parser FilePath
pursFileParser =
  Options.Applicative.strArgument
    ( Options.Applicative.metavar "FILE"
        <> Options.Applicative.help "The input .purs file(s)."
    )

verboseErrorsParser :: Options.Applicative.Parser Bool
verboseErrorsParser =
  Options.Applicative.switch
    ( Options.Applicative.short 'v'
        <> Options.Applicative.long "verbose-errors"
        <> Options.Applicative.help "Display verbose error messages"
    )

warnFileTypeNotFound ::
  String ->
  RIO SimpleApp ()
warnFileTypeNotFound message = do
  hPutBuilder stderr (fromString ("purs-compile: No files found using pattern: " <> message))
