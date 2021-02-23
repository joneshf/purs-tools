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

import qualified "error" Error
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "purescript" Language.PureScript.Names
import qualified "purescript" Language.PureScript.Options
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "pretty-simple" Text.Pretty.Simple

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
-- The different modes we can run in.
newtype Mode
  = -- |
    -- View an externs file
    View ViewMode

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
    View _ -> "View"

data ViewMode = ViewMode
  { declarations :: Bool,
    everything :: Bool,
    externsFile :: FilePath,
    -- |
    -- Whether to opt-out of any warnings.
    ignoreWarnings :: Bool,
    instances :: Bool
  }

instance Display ViewMode where
  display :: ViewMode -> Utf8Builder
  display mode = case mode of
    ViewMode {declarations, everything, externsFile, ignoreWarnings, instances} ->
      "ViewMode { "
        <> "declarations = "
        <> displayShow declarations
        <> ", "
        <> "everything = "
        <> displayShow everything
        <> ", "
        <> "externsFile = "
        <> fromString externsFile
        <> ", "
        <> "ignoreWarnings = "
        <> displayShow ignoreWarnings
        <> ", "
        <> "instances = "
        <> displayShow instances
        <> " }"

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
        <> Options.Applicative.progDesc "View a PureScript externs file"
        <> Options.Applicative.header "purs-externs - A PureScript externs file helper"

-- |
-- Our entry point to `purs-externs`.
main :: IO ()
main = do
  arguments <- Options.Applicative.execParser argumentsParserInfo
  case arguments of
    Arguments {mode, verbose} -> do
      logOptions <- logOptionsHandle stderr verbose
      withLogFunc logOptions \logFunc -> do
        simpleApp <- mkSimpleApp logFunc Nothing
        runRIO simpleApp do
          logDebugS "purs-externs" ("Processing Mode: " <> display mode)
          result <- case mode of
            View viewMode -> viewModeRun viewMode
          case result of
            Left error -> do
              Error.exit "purs-externs" error
            Right output -> do
              hPutBuilder stdout (getUtf8Builder output)
              exitSuccess

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  asum
    [ Options.Applicative.hsubparser
        ( fold
            [ Options.Applicative.command "view" (fmap View viewModeParserInfo)
            ]
        )
    ]

-- |
-- The actual parser for @Mode@.
viewModeParser :: Options.Applicative.Parser ViewMode
viewModeParser =
  pure ViewMode
    <*> declarations
    <*> everything
    <*> externsFile
    <*> ignoreWarnings
    <*> instances
  where
    declarations :: Options.Applicative.Parser Bool
    declarations =
      Options.Applicative.switch
        ( Options.Applicative.help "Display declarations"
            <> Options.Applicative.long "declarations"
        )

    everything :: Options.Applicative.Parser Bool
    everything =
      Options.Applicative.switch
        ( Options.Applicative.help "Display everything"
            <> Options.Applicative.long "everything"
        )

    externsFile :: Options.Applicative.Parser FilePath
    externsFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "The externs file to parse."
            <> Options.Applicative.long "externs-file"
            <> Options.Applicative.metavar "FILE"
        )

    ignoreWarnings :: Options.Applicative.Parser Bool
    ignoreWarnings =
      Options.Applicative.switch
        ( Options.Applicative.help "Don't let warnings fail compilation"
            <> Options.Applicative.long "ignore-warnings"
        )

    instances :: Options.Applicative.Parser Bool
    instances =
      Options.Applicative.switch
        ( Options.Applicative.help "Display typeclass instances"
            <> Options.Applicative.long "instances"
        )

-- |
-- This wraps the @parser@ with some more information and helper text.
viewModeParserInfo :: Options.Applicative.ParserInfo ViewMode
viewModeParserInfo = Options.Applicative.info viewModeParser description
  where
    description :: Options.Applicative.InfoMod ViewMode
    description =
      Options.Applicative.progDesc "View externs file"

viewModeRun ::
  ViewMode ->
  RIO SimpleApp (Either Error.Error Utf8Builder)
viewModeRun mode = case mode of
  ViewMode {externsFile, ignoreWarnings} -> do
    logDebugS "purs-externsFile" ("Processing view mode: " <> display mode)
    result <- liftIO do
      Language.PureScript.Make.runMake Language.PureScript.Options.defaultOptions do
        externs' <- Language.PureScript.Make.Monad.readExternsFile externsFile
        case externs' of
          Nothing -> exitFailure
          Just externs -> do
            when (declarations mode) do
              hPutBuilder stdout "Declarations\n"
              hPutBuilder stdout "============\n"
              for_ (Language.PureScript.Externs.efDeclarations externs) \declaration -> do
                hPutBuilder stdout "Declaration\n"
                hPutBuilder stdout "===========\n"
                Text.Pretty.Simple.pPrint declaration
            when (instances mode) do
              hPutBuilder stdout "Instances\n"
              hPutBuilder stdout "============\n"
              for_ (Language.PureScript.Externs.efDeclarations externs) \declaration -> do
                case declaration of
                  Language.PureScript.Externs.EDInstance {} -> do
                    hPutBuilder
                      stdout
                      ( getUtf8Builder
                          ( "Instance"
                              <> newline
                              <> "==========="
                              <> newline
                              <> "Name: "
                              <> displayIdent externs (Language.PureScript.Externs.edInstanceName declaration)
                              <> newline
                              <> "Type Class: "
                              <> displayQualifiedName (Language.PureScript.Externs.edInstanceClassName declaration)
                              <> newline
                          )
                      )
                    Text.Pretty.Simple.pPrint declaration
                  _ -> pure ()
            when (everything mode) do
              Text.Pretty.Simple.pPrint externs
            pure (Right ())
    pure (Error.fromRebuildModule ignoreWarnings result)

displayIdent ::
  Language.PureScript.Externs.ExternsFile ->
  Language.PureScript.Names.Ident ->
  Utf8Builder
displayIdent externsFile ident =
  display
    ( Language.PureScript.Names.runModuleName (Language.PureScript.Externs.efModuleName externsFile)
        <> "."
        <> Language.PureScript.Names.runIdent ident
    )

displayQualifiedName ::
  Language.PureScript.Names.Qualified (Language.PureScript.Names.ProperName a) ->
  Utf8Builder
displayQualifiedName qualified = display (Language.PureScript.Names.showQualified Language.PureScript.Names.runProperName qualified)

newline :: Utf8Builder
newline = "\n"
