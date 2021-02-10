{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CoreFn
  ( Mode,
    codegen,
    modeParser,
  )
where

import qualified "purescript" Control.Monad.Supply
import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.CoreFn.ToJSON
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)

-- |
-- The data for compiling CoreFn files.
newtype Mode = Mode
  { -- |
    -- The output CoreFn file we expect to generate.
    output :: FilePath
  }

instance Display Mode where
  display :: Mode -> Utf8Builder
  display coreFn = case coreFn of
    Mode {output} ->
      "CoreFn.Mode { "
        <> "output = "
        <> displayShow output
        <> " }"

-- |
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  pure Mode
    <*> output
  where
    output :: Options.Applicative.Parser FilePath
    output =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the compiled CoreFn file"
            <> Options.Applicative.long "output-corefn-file"
            <> Options.Applicative.metavar "FILE"
        )

codegen ::
  Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
  Mode ->
  Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
codegen coreFnModule coreFn = do
  let coreFnJSON = Language.PureScript.CoreFn.ToJSON.moduleToJSON Language.PureScript.version coreFnModule
  lift (Language.PureScript.Make.Monad.writeJSONFile (output coreFn) coreFnJSON)
