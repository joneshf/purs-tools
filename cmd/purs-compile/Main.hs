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
import qualified "purescript" Language.PureScript.ModuleDependencies
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
    includeDirectories :: [FilePath]
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
      Language.PureScript.Make.Actions.writeCacheDb' (outputDirectory compileOptions) (snd preCompiled)
      let preCompiledModuleNames = RIO.Map.keysSet (fst preCompiled)
      preCompiledExternsFiles <- sortExternsFiles (RIO.Map.elems (fst preCompiled))
      unCompiledModules <- Language.PureScript.CST.parseModulesFromFiles id moduleFiles
      let filePathMap = RIO.Map.fromList (fmap filePathPair unCompiledModules)
      foreigns <- Language.PureScript.Make.inferForeignModules filePathMap
      let makeActions = Language.PureScript.Make.Actions.buildMakeActions (outputDirectory compileOptions) filePathMap foreigns (usePrefix compileOptions)
      sortedModules <- Language.PureScript.ModuleDependencies.sortModules (partialResultModuleSignature preCompiledModuleNames) unCompiledModules
      compileModules makeActions preCompiledExternsFiles (fst sortedModules)
  printWarningsAndErrors (Language.PureScript.Options.optionsVerboseErrors (options compileOptions)) (jsonErrors compileOptions) makeWarnings makeErrors
  exitSuccess

compileModules ::
  Language.PureScript.Make.Actions.MakeActions Language.PureScript.Make.Monad.Make ->
  [Language.PureScript.Externs.ExternsFile] ->
  [(FilePath, Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module)] ->
  Language.PureScript.Make.Monad.Make [Language.PureScript.Externs.ExternsFile]
compileModules makeActions externsFiles notCompiledModules' = case notCompiledModules' of
  [] -> pure externsFiles
  notCompiledModule : notCompiledModules -> do
    module' <- Language.PureScript.CST.unwrapParserError (fst notCompiledModule) (Language.PureScript.CST.Parser.resFull (snd notCompiledModule))
    let Language.PureScript.AST.Declarations.Module _ _ moduleName _ _ = module'
    cacheInfo <- getCacheInfo makeActions moduleName
    externsFile <- Language.PureScript.Make.rebuildModule makeActions externsFiles module'
    oldCacheDb <- Language.PureScript.Make.Actions.readCacheDb makeActions
    let newCacheDb = RIO.Map.insert moduleName cacheInfo oldCacheDb
    Language.PureScript.Make.Actions.writeCacheDb makeActions newCacheDb
    compileModules makeActions (externsFiles <> [externsFile]) notCompiledModules

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

getCacheInfo ::
  Language.PureScript.Make.Actions.MakeActions Language.PureScript.Make.Monad.Make ->
  Language.PureScript.Names.ModuleName ->
  Language.PureScript.Make.Monad.Make Language.PureScript.Make.Cache.CacheInfo
getCacheInfo makeActions moduleName = do
  inputTimestampsAndHashes <- Language.PureScript.Make.Actions.getInputTimestampsAndHashes makeActions moduleName
  case inputTimestampsAndHashes of
    Left _ -> do
      pure (Language.PureScript.Make.Cache.CacheInfo RIO.Map.empty)
    Right cacheInfoMap' -> do
      cacheInfoMap <- for cacheInfoMap' \info -> do
        contentHash <- snd info
        pure (fst info, contentHash)
      pure (Language.PureScript.Make.Cache.CacheInfo cacheInfoMap)

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

partialResultModuleSignature ::
  forall a.
  RIO.Set.Set Language.PureScript.Names.ModuleName ->
  (a, Language.PureScript.CST.Parser.PartialResult Language.PureScript.AST.Declarations.Module) ->
  Language.PureScript.ModuleDependencies.ModuleSignature
partialResultModuleSignature preCompiledModuleNames partialResultModule = case Language.PureScript.CST.Parser.resPartial (snd partialResultModule) of
  Language.PureScript.AST.Declarations.Module sourceSpan _ moduleName declarations _ ->
    Language.PureScript.ModuleDependencies.ModuleSignature
      sourceSpan
      moduleName
      (RIO.Map.toList (foldMap (moduleSignatureImports preCompiledModuleNames) declarations))

moduleSignatureImports ::
  RIO.Set.Set Language.PureScript.Names.ModuleName ->
  Language.PureScript.AST.Declarations.Declaration ->
  RIO.Map.Map
    Language.PureScript.Names.ModuleName
    Language.PureScript.AST.SourcePos.SourceSpan
moduleSignatureImports preCompiledModuleNames declaration = case declaration of
  Language.PureScript.AST.Declarations.BindingGroupDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.BoundValueDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.DataBindingGroupDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.DataDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.ExternDataDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.ExternDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.ExternKindDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.FixityDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.ImportDeclaration sourceAnn moduleName _ _ ->
    if RIO.Set.member moduleName preCompiledModuleNames
      then RIO.Map.empty
      else RIO.Map.singleton moduleName (fst sourceAnn)
  Language.PureScript.AST.Declarations.TypeClassDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.TypeDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.TypeInstanceDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.TypeSynonymDeclaration {} -> RIO.Map.empty
  Language.PureScript.AST.Declarations.ValueDeclaration {} -> RIO.Map.empty

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

sortExternsFiles ::
  [Language.PureScript.Externs.ExternsFile] ->
  Language.PureScript.Make.Monad.Make [Language.PureScript.Externs.ExternsFile]
sortExternsFiles externsFiles = do
  externsFilesAndGraph <- Language.PureScript.ModuleDependencies.sortModules externsModuleSignature externsFiles
  pure (fst externsFilesAndGraph)

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
