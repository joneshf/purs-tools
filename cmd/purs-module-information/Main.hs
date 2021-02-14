{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified "cst" CST
import qualified "aeson" Data.Aeson
import qualified "purescript-cst" Language.PureScript.CST.Types
import qualified "purescript-ast" Language.PureScript.Names
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.List

-- |
-- The arguments to the program.
data Arguments = Arguments
  { -- |
    -- The actual 'Mode' we want to run.
    mode :: Mode,
    -- |
    -- Whether we want to display debug information or not.
    verbose :: Bool
  }

-- |
-- The different modes we can run in.
newtype Mode
  = -- |
    -- Display the module information
    Display DisplayMode

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
    Display _ -> "Display"

newtype DisplayMode = DisplayMode
  { -- |
    -- | The PureScript module to view.
    pursFile :: FilePath
  }

instance Display DisplayMode where
  display :: DisplayMode -> Utf8Builder
  display mode = case mode of
    DisplayMode {pursFile} ->
      "DisplayMode { "
        <> "pursFile = "
        <> fromString pursFile
        <> " }"

-- |
-- Information about a PureScript module.
data ModuleInformation = ModuleInformation
  { -- |
    -- Whether the module defines any typeclass instances.
    definesInstances :: Bool,
    -- |
    -- All of the modules this module imports.
    imports :: [Utf8Builder],
    -- |
    -- The name of the module.
    moduleName :: Utf8Builder,
    -- |
    -- All of the modules this module re-exports.
    reExports :: [Utf8Builder]
  }

instance Display ModuleInformation where
  display :: ModuleInformation -> Utf8Builder
  display moduleInformation' = case moduleInformation' of
    ModuleInformation {} ->
      "ModuleInformation { "
        <> "definesInstances = "
        <> displayShow (definesInstances moduleInformation')
        <> ", "
        <> "imports = ["
        <> fold (RIO.List.intersperse ", " (imports moduleInformation'))
        <> "]"
        <> ", "
        <> "moduleName = "
        <> moduleName moduleInformation'
        <> ", "
        <> "reExports = ["
        <> fold (RIO.List.intersperse ", " (reExports moduleInformation'))
        <> "]"
        <> " }"

-- |
-- The actual parser for 'Arguments'.
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
-- This wraps the 'argumentsParser' with some more information and helper text.
argumentsParserInfo :: Options.Applicative.ParserInfo Arguments
argumentsParserInfo =
  Options.Applicative.info (Options.Applicative.helper <*> argumentsParser) description
  where
    description :: Options.Applicative.InfoMod Arguments
    description =
      Options.Applicative.fullDesc
        <> Options.Applicative.progDesc "Display information about a PureScript module"
        <> Options.Applicative.header "purs-module-info - A PureScript module inspector"

cstModuleName ::
  Language.PureScript.CST.Types.Name Language.PureScript.Names.ModuleName ->
  Utf8Builder
cstModuleName name = display (Language.PureScript.Names.runModuleName (Language.PureScript.CST.Types.nameValue name))

debug ::
  Utf8Builder ->
  RIO SimpleApp ()
debug message = do
  logDebugS "purs-module-info" message

-- |
-- The actual parser for 'Mode'.
displayModeParser :: Options.Applicative.Parser DisplayMode
displayModeParser =
  pure DisplayMode
    <*> pursFile
  where
    pursFile :: Options.Applicative.Parser FilePath
    pursFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "The PureScript module to inspect."
            <> Options.Applicative.long "purs-file"
            <> Options.Applicative.metavar "FILE"
        )

-- |
-- This wraps the 'parser' with some more information and helper text.
displayModeParserInfo :: Options.Applicative.ParserInfo DisplayMode
displayModeParserInfo = Options.Applicative.info displayModeParser description
  where
    description :: Options.Applicative.InfoMod DisplayMode
    description =
      Options.Applicative.progDesc "Display module information"

displayModeRun ::
  DisplayMode ->
  RIO SimpleApp ()
displayModeRun mode = case mode of
  DisplayMode {} -> do
    debug ("Processing display mode: " <> display mode)
    withLazyFile (pursFile mode) \contents -> do
      debug "Got file contents"
      case CST.parse contents of
        Left errors -> do
          debug "Got errors"
          hPutBuilder stderr (getUtf8Builder errors)
        Right cstModule -> do
          debug "Got a module"
          debug (display (moduleInformation cstModule))
          hPutBuilder stdout (renderJSON (moduleInformation cstModule))

importModuleName ::
  forall a.
  Language.PureScript.CST.Types.ImportDecl a ->
  Utf8Builder
importModuleName importDecl = cstModuleName (Language.PureScript.CST.Types.impModule importDecl)

isInstance ::
  forall a.
  Language.PureScript.CST.Types.Declaration a ->
  Bool
isInstance declaration = case declaration of
  Language.PureScript.CST.Types.DeclClass {} -> False
  Language.PureScript.CST.Types.DeclData {} -> False
  Language.PureScript.CST.Types.DeclDerive {} -> False
  Language.PureScript.CST.Types.DeclFixity {} -> False
  Language.PureScript.CST.Types.DeclForeign {} -> False
  Language.PureScript.CST.Types.DeclInstanceChain {} -> True
  Language.PureScript.CST.Types.DeclNewtype {} -> False
  Language.PureScript.CST.Types.DeclSignature {} -> False
  Language.PureScript.CST.Types.DeclType {} -> False
  Language.PureScript.CST.Types.DeclValue {} -> False

-- |
-- Our entry point to `purs-module-info`.
main :: IO ()
main = do
  arguments <- Options.Applicative.execParser argumentsParserInfo
  case arguments of
    Arguments {mode, verbose} -> do
      logOptions <- logOptionsHandle stderr verbose
      withLogFunc logOptions \logFunc -> do
        simpleApp <- mkSimpleApp logFunc Nothing
        runRIO simpleApp do
          debug ("Processing Mode: " <> display mode)
          case mode of
            Display displayMode -> displayModeRun displayMode

-- |
-- The actual parser for 'Mode'.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  asum
    [ Options.Applicative.hsubparser
        ( fold
            [ Options.Applicative.command "info" (fmap Display displayModeParserInfo)
            ]
        ),
      fmap Display displayModeParser
    ]

moduleInformation ::
  forall a.
  Language.PureScript.CST.Types.Module a ->
  ModuleInformation
moduleInformation cstModule =
  ModuleInformation
    { definesInstances = any isInstance (Language.PureScript.CST.Types.modDecls cstModule),
      imports = fmap importModuleName (Language.PureScript.CST.Types.modImports cstModule),
      moduleName = cstModuleName (Language.PureScript.CST.Types.modNamespace cstModule),
      reExports = foldMap reExportModuleNames (Language.PureScript.CST.Types.modExports cstModule)
    }

reExportModuleName ::
  forall a.
  Language.PureScript.CST.Types.Export a ->
  [Utf8Builder]
reExportModuleName export' = case export' of
  Language.PureScript.CST.Types.ExportClass {} -> []
  Language.PureScript.CST.Types.ExportKind {} -> []
  Language.PureScript.CST.Types.ExportModule _ _ name -> [cstModuleName name]
  Language.PureScript.CST.Types.ExportOp {} -> []
  Language.PureScript.CST.Types.ExportType {} -> []
  Language.PureScript.CST.Types.ExportTypeOp {} -> []
  Language.PureScript.CST.Types.ExportValue {} -> []

reExportModuleNames ::
  forall a.
  Language.PureScript.CST.Types.DelimitedNonEmpty (Language.PureScript.CST.Types.Export a) ->
  [Utf8Builder]
reExportModuleNames export' = case Language.PureScript.CST.Types.wrpValue export' of
  export -> foldMap reExportModuleName export

renderJSON ::
  ModuleInformation ->
  Builder
renderJSON moduleInformation' =
  Data.Aeson.fromEncoding
    ( Data.Aeson.pairs
        ( "definesInstances" Data.Aeson..= definesInstances moduleInformation'
            <> "imports" Data.Aeson..= RIO.List.sort (fmap utf8BuilderToText (imports moduleInformation'))
            <> "moduleName" Data.Aeson..= utf8BuilderToText (moduleName moduleInformation')
            <> "reExports" Data.Aeson..= RIO.List.sort (fmap utf8BuilderToText (reExports moduleInformation'))
        )
    )
