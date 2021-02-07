{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Version
  ( Mode,
    modeParserInfo,
    modeRun,
  )
where

import qualified "base" Data.Version
import qualified "this" Error
import qualified "purescript" Language.PureScript
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)

-- |
-- The different modes for `version`.
data Mode
  = -- |
    -- The "human" mode is intended to provide more information that humans might find useful.
    Human
  | -- |
    -- The "numeric" mode is intended for easier to parse information that a program might consume.
    Numeric

instance Display Mode where
  display :: Mode -> Utf8Builder
  display mode = case mode of
    Human -> "Human"
    Numeric -> "Numeric"

-- |
-- A helper for creating a newline.
newline :: Utf8Builder
newline = "\n"

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  asum
    [ pure Human,
      Options.Applicative.flag' Numeric numeric
    ]
  where
    numeric :: Options.Applicative.Mod Options.Applicative.FlagFields a
    numeric =
      Options.Applicative.help "Print machine-readable version number only"
        <> Options.Applicative.long "numeric"

-- |
-- This wraps the @modeParser@ with some more information and helper text.
modeParserInfo :: Options.Applicative.ParserInfo Mode
modeParserInfo = Options.Applicative.info modeParser description
  where
    description :: Options.Applicative.InfoMod Mode
    description =
      Options.Applicative.progDesc "Print version information"

-- |
-- We handle the different alternates of @VersionMode@ and run each one appropriately.
modeRun ::
  Mode ->
  RIO SimpleApp (Either Error.Error Utf8Builder)
modeRun mode = do
  logDebugS "purs-compile-module" ("Processing version mode: " <> display mode)
  case mode of
    Human -> runHuman
    Numeric -> runNumeric

-- |
-- We want to provide a few versions for humans:
-- - The version of our `purs-compile-module` program.
-- - The version of the underlying `purs` program.
--
-- This is because people generally need this information when they're diagnosing issues.
runHuman :: RIO SimpleApp (Either Error.Error Utf8Builder)
runHuman = do
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
runNumeric :: RIO SimpleApp (Either Error.Error Utf8Builder)
runNumeric = do
  logDebugS "purs-compile-module" "Rendering numeric version"
  pure
    ( Right
        ( fromString (Data.Version.showVersion version)
            <> newline
        )
    )

-- |
-- The version of `purs-compile-module`.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 0, 0]
