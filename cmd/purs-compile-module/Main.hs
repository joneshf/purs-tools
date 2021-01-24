{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified "base" Data.Version
import qualified "purescript" Language.PureScript
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)

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
-- Any errors we might run into that we want to return at the end of the program.
data Error

instance Display Error where
  display :: Error -> Utf8Builder
  display error = case error of

-- |
-- The different modes we can run in.
--
-- TODO(joneshf): We want to add a "compile" mode.
newtype Mode
  = -- |
    -- Show the version
    Version VersionMode

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
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
            Version versionMode -> versionModeRun versionMode
          case result of
            Left error -> do
              logErrorS "purs-compile-module" (display error)
              exitFailure
            Right output -> do
              hPutBuilder stdout (getUtf8Builder output)
              exitSuccess

-- |
-- The actual parser for @Mode@.
--
-- TODO(joneshf): This should have a default of compiling without having to explicitly state you want to compile.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  Options.Applicative.hsubparser
    ( fold
        [ Options.Applicative.command "version" (fmap Version versionModeParserInfo)
        ]
    )

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
