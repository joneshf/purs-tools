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

import qualified "compile-module" CompileModule
import qualified "compile-module" Error
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "compile-module" Version

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
data Mode
  = -- |
    -- Compile a module
    Compile CompileModule.Mode
  | -- |
    -- Show the version
    Version Version.Mode

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
    Compile _ -> "Compile"
    Version _ -> "Version"

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
            Compile compileMode -> CompileModule.modeRun compileMode
            Version versionMode -> Version.modeRun versionMode
          case result of
            Left error -> Error.exit "purs-compile-module" error
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
            [ Options.Applicative.command "compile" (fmap Compile CompileModule.modeParserInfo),
              Options.Applicative.command "version" (fmap Version Version.modeParserInfo)
            ]
        ),
      fmap Compile CompileModule.modeParser
    ]
