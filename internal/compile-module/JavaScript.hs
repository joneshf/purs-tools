{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JavaScript
  ( Mode,
    codegen,
    modeParser,
  )
where

import qualified "mtl" Control.Monad.Error.Class
import qualified "purescript" Control.Monad.Supply
import qualified "purescript" Language.PureScript.CodeGen.JS
import qualified "purescript" Language.PureScript.CodeGen.JS.Printer
import qualified "purescript" Language.PureScript.CoreFn.Ann
import qualified "purescript" Language.PureScript.CoreFn.Module
import qualified "purescript" Language.PureScript.CoreImp.AST
import qualified "purescript" Language.PureScript.Errors
import qualified "purescript" Language.PureScript.Make.Actions
import qualified "purescript" Language.PureScript.Make.Monad
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (error)
import qualified "rio" RIO.FilePath

-- |
-- The data for compiling FFI files.
data FFIFiles = FFIFiles
  { -- |
    -- The input FFI file we depend on.
    inputFFIFile :: FilePath,
    -- |
    -- The output FFI file we expect to generate.
    outputFFIFile :: FilePath
  }

instance Display FFIFiles where
  display :: FFIFiles -> Utf8Builder
  display ffi = case ffi of
    FFIFiles {inputFFIFile, outputFFIFile} ->
      "JavaScript.FFIFiles { "
        <> "inputFFIFile = "
        <> displayShow inputFFIFile
        <> ", "
        <> "outputFFIFile = "
        <> displayShow outputFFIFile
        <> " }"

-- |
-- The data for compiling JavaScript files.
data Mode = Mode
  { -- |
    -- Any FFI files we deal with.
    ffiFiles :: Maybe FFIFiles,
    -- |
    -- The output JavaScript file we expect to generate.
    outputJavaScriptFile :: FilePath
  }

instance Display Mode where
  display :: Mode -> Utf8Builder
  display javaScript = case javaScript of
    Mode {ffiFiles = ffiFiles', outputJavaScriptFile} ->
      "JavaScript.Mode { "
        <> foldMap
          ( \ffiFiles ->
              "ffiFiles = "
                <> display ffiFiles
                <> ", "
          )
          ffiFiles'
        <> "outputJavaScriptFile = "
        <> displayShow outputJavaScriptFile
        <> " }"

codegen ::
  Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
  Mode ->
  Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make ()
codegen coreFnModule mode = do
  ffiModule <- codegenFFI coreFnModule mode
  coreImpASTs <- Language.PureScript.CodeGen.JS.moduleToJs coreFnModule ffiModule
  lift (Language.PureScript.Make.Monad.writeTextFile (outputJavaScriptFile mode) (encodeUtf8 (Language.PureScript.CodeGen.JS.Printer.prettyPrintJS coreImpASTs)))

codegenFFI ::
  Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
  Mode ->
  Control.Monad.Supply.SupplyT Language.PureScript.Make.Monad.Make (Maybe Language.PureScript.CoreImp.AST.AST)
codegenFFI coreFnModule mode = do
  lift (validateFFIModule coreFnModule (ffiFiles mode))
  for (ffiFiles mode) \ffi -> do
    lift (Language.PureScript.Make.Monad.copyFile (inputFFIFile ffi) (outputFFIFile ffi))
    pure
      ( Language.PureScript.CoreImp.AST.App
          Nothing
          (Language.PureScript.CoreImp.AST.Var Nothing "require")
          [ Language.PureScript.CoreImp.AST.StringLiteral
              Nothing
              ( fromString
                  ( importRelativeTo
                      (outputFFIFile ffi)
                      (outputJavaScriptFile mode)
                  )
              )
          ]
      )

-- |
-- The actual parser for @FFIFiles@.
ffiFilesParser :: Options.Applicative.Parser FFIFiles
ffiFilesParser =
  pure FFIFiles
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
-- The actual parser for @Mode@.
modeParser :: Options.Applicative.Parser Mode
modeParser =
  pure Mode
    <*> optional ffiFilesParser
    <*> outputJavaScriptFile
  where
    outputJavaScriptFile :: Options.Applicative.Parser FilePath
    outputJavaScriptFile =
      Options.Applicative.strOption
        ( Options.Applicative.help "Where to place the compiled JavaScript file"
            <> Options.Applicative.long "output-javascript-file"
            <> Options.Applicative.metavar "FILE"
        )

validateFFIModule ::
  Language.PureScript.CoreFn.Module.Module Language.PureScript.CoreFn.Ann.Ann ->
  Maybe FFIFiles ->
  Language.PureScript.Make.Monad.Make ()
validateFFIModule coreFnModule ffi' = do
  case (Language.PureScript.CoreFn.Module.moduleForeign coreFnModule, ffi') of
    ([], Just ffi) -> do
      Control.Monad.Error.Class.throwError
        ( Language.PureScript.Errors.errorMessage'
            (Language.PureScript.CoreFn.Module.moduleSourceSpan coreFnModule)
            ( Language.PureScript.Errors.UnnecessaryFFIModule
                (Language.PureScript.CoreFn.Module.moduleName coreFnModule)
                (inputFFIFile ffi)
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
    (_, Just ffi) -> do
      Language.PureScript.Make.Actions.checkForeignDecls coreFnModule (inputFFIFile ffi)
