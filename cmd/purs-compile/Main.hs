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
import qualified "purescript" Language.PureScript.AST.SourcePos
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Parser
import qualified "purescript" Language.PureScript.Errors
import qualified "purescript" Language.PureScript.Errors.JSON
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Cache
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.ByteString.Lazy
import qualified "rio" RIO.Directory
import "rio" RIO.FilePath ((</>))
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
    jsonErrors :: Bool,
    includeDirectories :: [FilePath],
    strict :: Bool
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
      cacheDb <- Language.PureScript.Make.Actions.readCacheDb' (outputDirectory compileOptions)
      preCompiled <- includeAllPreCompiledArtifacts (outputDirectory compileOptions) cacheDb (includeDirectories compileOptions)
      let preCompiledFilePathAndModuleMap = fmap preCompiledFilePathPair (fst preCompiled)
      unCompiledModules <- Language.PureScript.CST.parseModulesFromFiles id moduleFiles
      let unCompiledFilePathAndModuleMap = RIO.Map.fromList (fmap unCompiledFilePathPair unCompiledModules)
      let filePathAndModuleMap = RIO.Map.union unCompiledFilePathAndModuleMap preCompiledFilePathAndModuleMap
      let filePathMap = fmap fst filePathAndModuleMap
      let moduleMap = fmap snd filePathAndModuleMap
      foreigns <- Language.PureScript.Make.inferForeignModules filePathMap
      let makeActions = Language.PureScript.Make.Actions.buildMakeActions (outputDirectory compileOptions) filePathMap foreigns (usePrefix compileOptions)
      _ <- Language.PureScript.Make.make makeActions (RIO.Map.elems moduleMap)
      -- N.B. `Language.PureScript.Make.make` will remove anything from the cache marked as `Language.PureScript.Make.Actions.RebuildNever`.
      -- Since we want the pre-compiled artifacts to be available for future compilations,
      -- we write that data to tthe cache after `Language.PureScript.Make.make` has removed them (if they were there).
      --
      -- It seems like a bug that it removes something that was marked `Language.PureScript.Make.Actions.RebuildNever`.
      -- It doesn't seem like it should remove anything.
      -- But, it might also be intentional for some reason.
      newCacheDb <- Language.PureScript.Make.Actions.readCacheDb makeActions
      Language.PureScript.Make.Actions.writeCacheDb makeActions (RIO.Map.union newCacheDb (snd preCompiled))
  printWarningsAndErrors (strict compileOptions) (Language.PureScript.Options.optionsVerboseErrors (options compileOptions)) (jsonErrors compileOptions) makeWarnings makeErrors
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
    <*> many includeDirectoryParser
    <*> strictParser

compileOptionsParserInfo :: Options.Applicative.ParserInfo CompileOptions
compileOptionsParserInfo =
  Options.Applicative.info
    (Options.Applicative.helper <*> compileOptionsParser)
    compileOptionsDescription

copyCoreFnJSON ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyCoreFnJSON includeDirectory outputDirectory' moduleName =
  whenM (RIO.Directory.doesFileExist (resolveCoreFnJSON includeDirectory moduleName)) do
    Language.PureScript.Make.Monad.copyFile
      (resolveCoreFnJSON includeDirectory moduleName)
      (resolveCoreFnJSON outputDirectory' moduleName)

copyDocsJSON ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyDocsJSON includeDirectory outputDirectory' moduleName =
  whenM (RIO.Directory.doesFileExist (resolveDocsJSON includeDirectory moduleName)) do
    Language.PureScript.Make.Monad.copyFile
      (resolveDocsJSON includeDirectory moduleName)
      (resolveDocsJSON outputDirectory' moduleName)

copyExternsFile ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyExternsFile includeDirectory outputDirectory' moduleName =
  Language.PureScript.Make.Monad.copyFile
    (resolveExternsFile includeDirectory moduleName)
    (resolveExternsFile outputDirectory' moduleName)

copyForeignJS ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyForeignJS includeDirectory outputDirectory' moduleName =
  whenM (RIO.Directory.doesFileExist (resolveForeignJS includeDirectory moduleName)) do
    Language.PureScript.Make.Monad.copyFile
      (resolveForeignJS includeDirectory moduleName)
      (resolveForeignJS outputDirectory' moduleName)

copyIndexJS ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyIndexJS includeDirectory outputDirectory' moduleName =
  whenM (RIO.Directory.doesFileExist (resolveIndexJS includeDirectory moduleName)) do
    Language.PureScript.Make.Monad.copyFile
      (resolveIndexJS includeDirectory moduleName)
      (resolveIndexJS outputDirectory' moduleName)

copyIndexJSMap ::
  FilePath ->
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make ()
copyIndexJSMap includeDirectory outputDirectory' moduleName =
  whenM (RIO.Directory.doesFileExist (resolveIndexJSMap includeDirectory moduleName)) do
    Language.PureScript.Make.Monad.copyFile
      (resolveIndexJSMap includeDirectory moduleName)
      (resolveIndexJSMap outputDirectory' moduleName)

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

includeAllPreCompiledArtifacts ::
  FilePath ->
  Language.PureScript.Make.Cache.CacheDb ->
  [FilePath] ->
  Language.PureScript.Make.Monad.Make
    ( RIO.Map.Map Language.PureScript.Names.ModuleName Language.PureScript.Externs.ExternsFile,
      Language.PureScript.Make.Cache.CacheDb
    )
includeAllPreCompiledArtifacts outputDirectory' oldCacheDb includeDirectories' = do
  externsFileMap <- flip RIO.Map.traverseMaybeWithKey oldCacheDb \moduleName _ -> do
    readExternsFile outputDirectory' moduleName
  foldM
    (includePreCompiledArtifacts outputDirectory')
    (externsFileMap, oldCacheDb)
    includeDirectories'

includePreCompiledArtifacts ::
  FilePath ->
  ( RIO.Map.Map Language.PureScript.Names.ModuleName Language.PureScript.Externs.ExternsFile,
    Language.PureScript.Make.Cache.CacheDb
  ) ->
  FilePath ->
  Language.PureScript.Make.Monad.Make
    ( RIO.Map.Map Language.PureScript.Names.ModuleName Language.PureScript.Externs.ExternsFile,
      Language.PureScript.Make.Cache.CacheDb
    )
includePreCompiledArtifacts outputDirectory' result includeDirectory = do
  includeCacheDb <- Language.PureScript.Make.Actions.readCacheDb' includeDirectory
  newCacheDb <- flip RIO.Map.traverseMaybeWithKey includeCacheDb \moduleName newCacheInfo -> do
    case RIO.Map.lookup moduleName (snd result) of
      Just _ -> do
        pure Nothing
      Nothing -> do
        copyCoreFnJSON includeDirectory outputDirectory' moduleName
        copyDocsJSON includeDirectory outputDirectory' moduleName
        copyExternsFile includeDirectory outputDirectory' moduleName
        copyForeignJS includeDirectory outputDirectory' moduleName
        copyIndexJS includeDirectory outputDirectory' moduleName
        copyIndexJSMap includeDirectory outputDirectory' moduleName
        externsFile' <- readExternsFile includeDirectory moduleName
        case externsFile' of
          Nothing -> pure Nothing
          Just externsFile -> pure (Just (externsFile, newCacheInfo))
  pure
    ( bimap
        (RIO.Map.union (fmap fst newCacheDb))
        (RIO.Map.union (fmap snd newCacheDb))
        result
    )

includeDirectoryParser :: Options.Applicative.Parser FilePath
includeDirectoryParser =
  Options.Applicative.strOption
    ( Options.Applicative.long "include"
        <> Options.Applicative.help
          ( "A directory of pre-compiled artifacts to include."
              <> "This flag can be used multiple times to include multiple directories."
          )
    )

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

preCompiledFilePathPair ::
  forall a.
  Language.PureScript.Externs.ExternsFile ->
  ( Either Language.PureScript.Make.Actions.RebuildPolicy a,
    Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module
  )
preCompiledFilePathPair externsFile =
  ( Left Language.PureScript.Make.Actions.RebuildNever,
    preCompiledModule externsFile
  )

preCompiledImport ::
  Language.PureScript.Externs.ExternsImport ->
  Language.PureScript.AST.Declarations.Declaration
preCompiledImport externsImport =
  Language.PureScript.AST.Declarations.ImportDeclaration
    Language.PureScript.AST.SourcePos.nullSourceAnn
    (Language.PureScript.Externs.eiModule externsImport)
    (Language.PureScript.Externs.eiImportType externsImport)
    (Language.PureScript.Externs.eiImportedAs externsImport)

preCompiledModule ::
  Language.PureScript.Externs.ExternsFile ->
  Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module
preCompiledModule externsFile =
  Language.PureScript.CST.Parser.PartialResult
    { Language.PureScript.CST.Parser.resFull = Right module',
      Language.PureScript.CST.Parser.resPartial = module'
    }
  where
    module' :: Language.PureScript.AST.Declarations.Module
    module' =
      Language.PureScript.AST.Declarations.Module
        (Language.PureScript.Externs.efSourceSpan externsFile)
        []
        (Language.PureScript.Externs.efModuleName externsFile)
        (fmap preCompiledImport (Language.PureScript.Externs.efImports externsFile))
        (Just (Language.PureScript.Externs.efExports externsFile))

-- | Arguments: ignore errors, verbose, use JSON, warnings, errors
printWarningsAndErrors ::
  forall a.
  Bool ->
  Bool ->
  Bool ->
  Language.PureScript.Errors.MultipleErrors ->
  Either Language.PureScript.Errors.MultipleErrors a ->
  RIO SimpleApp ()
printWarningsAndErrors strict' verbose useJSON warnings errors'
  | useJSON = do
    RIO.ByteString.Lazy.hPut stderr (Data.Aeson.encode (jsonResult verbose warnings errors'))
    case errors' of
      Left _ -> exitFailure
      Right _ -> do
        when (strict' && Language.PureScript.Errors.nonEmpty warnings) do
          exitFailure
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
        when (strict' && Language.PureScript.Errors.nonEmpty warnings) do
          exitFailure

pursFileParser :: Options.Applicative.Parser FilePath
pursFileParser =
  Options.Applicative.strArgument
    ( Options.Applicative.metavar "FILE"
        <> Options.Applicative.help "The input .purs file(s)."
    )

readExternsFile ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make (Maybe Language.PureScript.Externs.ExternsFile)
readExternsFile directory moduleName = do
  Language.PureScript.Make.Monad.readExternsFile (resolveExternsFile directory moduleName)

resolveCoreFnJSON ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveCoreFnJSON directory moduleName =
  resolveFilePath directory moduleName "corefn.json"

resolveDocsJSON ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveDocsJSON directory moduleName =
  resolveFilePath directory moduleName "docs.json"

resolveExternsFile ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveExternsFile directory moduleName =
  resolveFilePath directory moduleName Language.PureScript.Externs.externsFileName

resolveFilePath ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath ->
  FilePath
resolveFilePath directory moduleName filename =
  directory
    </> RIO.Text.unpack (Language.PureScript.Names.runModuleName moduleName)
    </> filename

resolveForeignJS ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveForeignJS directory moduleName =
  resolveFilePath directory moduleName "foreign.js"

resolveIndexJS ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveIndexJS directory moduleName =
  resolveFilePath directory moduleName "index.js"

resolveIndexJSMap ::
  FilePath ->
  Language.PureScript.Names.ModuleName ->
  FilePath
resolveIndexJSMap directory moduleName =
  resolveFilePath directory moduleName "index.js.map"

strictParser :: Options.Applicative.Parser Bool
strictParser =
  Options.Applicative.switch
    ( Options.Applicative.help "Promote warnings to errors"
        <> Options.Applicative.long "strict"
    )

unCompiledFilePathPair ::
  forall a.
  (FilePath, Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module) ->
  ( Language.PureScript.Names.ModuleName,
    ( Either a FilePath,
      Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module
    )
  )
unCompiledFilePathPair (filePath, module') =
  ( Language.PureScript.AST.Declarations.getModuleName (Language.PureScript.CST.Parser.resPartial module'),
    ( Right filePath,
      module'
    )
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
