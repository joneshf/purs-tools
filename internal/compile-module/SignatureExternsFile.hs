{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SignatureExternsFile
  ( codegen,
  )
where

import qualified "purescript" Language.PureScript.AST.Declarations
import qualified "purescript" Language.PureScript.AST.SourcePos
import qualified "purescript" Language.PureScript.Environment
import qualified "purescript" Language.PureScript.Externs
import qualified "purescript" Language.PureScript.Make.Monad
import "rio" RIO hiding (error)

codegen ::
  Language.PureScript.Externs.ExternsFile ->
  FilePath ->
  Language.PureScript.Make.Monad.Make ()
codegen externsFile' outputExternsFile = do
  let externsFile :: Language.PureScript.Externs.ExternsFile
      externsFile = makeSignatureExternsFile externsFile'
  Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile

makeSignatureDeclarationRef ::
  Language.PureScript.AST.Declarations.DeclarationRef ->
  Language.PureScript.AST.Declarations.DeclarationRef
makeSignatureDeclarationRef declarationRef' = case declarationRef' of
  Language.PureScript.AST.Declarations.KindRef _ kindName ->
    Language.PureScript.AST.Declarations.KindRef Language.PureScript.AST.SourcePos.nullSourceSpan kindName
  Language.PureScript.AST.Declarations.ModuleRef _ moduleName ->
    Language.PureScript.AST.Declarations.ModuleRef Language.PureScript.AST.SourcePos.nullSourceSpan moduleName
  Language.PureScript.AST.Declarations.ReExportRef _ exportSource declarationRef ->
    Language.PureScript.AST.Declarations.ReExportRef Language.PureScript.AST.SourcePos.nullSourceSpan exportSource (makeSignatureDeclarationRef declarationRef)
  Language.PureScript.AST.Declarations.TypeClassRef _ className ->
    Language.PureScript.AST.Declarations.TypeClassRef Language.PureScript.AST.SourcePos.nullSourceSpan className
  Language.PureScript.AST.Declarations.TypeInstanceRef _ instanceName ->
    Language.PureScript.AST.Declarations.TypeInstanceRef Language.PureScript.AST.SourcePos.nullSourceSpan instanceName
  Language.PureScript.AST.Declarations.TypeOpRef _ opName ->
    Language.PureScript.AST.Declarations.TypeOpRef Language.PureScript.AST.SourcePos.nullSourceSpan opName
  Language.PureScript.AST.Declarations.TypeRef _ typeName constructorNames ->
    Language.PureScript.AST.Declarations.TypeRef Language.PureScript.AST.SourcePos.nullSourceSpan typeName constructorNames
  Language.PureScript.AST.Declarations.ValueOpRef _ opName ->
    Language.PureScript.AST.Declarations.ValueOpRef Language.PureScript.AST.SourcePos.nullSourceSpan opName
  Language.PureScript.AST.Declarations.ValueRef _ ident ->
    Language.PureScript.AST.Declarations.ValueRef Language.PureScript.AST.SourcePos.nullSourceSpan ident

makeSignatureExternsDeclaration ::
  Language.PureScript.Externs.ExternsDeclaration ->
  Language.PureScript.Externs.ExternsDeclaration
makeSignatureExternsDeclaration externsDeclaration = case externsDeclaration of
  Language.PureScript.Externs.EDClass name typeArguments members constraints fundeps isEmpty ->
    Language.PureScript.Externs.EDClass
      name
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) typeArguments)
      ((fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) members)
      (fmap (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constraints)
      fundeps
      isEmpty
  Language.PureScript.Externs.EDDataConstructor name origin typeConstructor type' fields ->
    Language.PureScript.Externs.EDDataConstructor
      name
      origin
      typeConstructor
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')
      fields
  Language.PureScript.Externs.EDInstance className name types constraints chain chainIndex ->
    Language.PureScript.Externs.EDInstance
      className
      name
      (fmap (Language.PureScript.AST.SourcePos.nullSourceAnn <$) types)
      ((fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constraints)
      chain
      chainIndex
  Language.PureScript.Externs.EDKind name ->
    Language.PureScript.Externs.EDKind name
  Language.PureScript.Externs.EDType name kind declarationKind ->
    Language.PureScript.Externs.EDType
      name
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ kind)
      (makeSignatureTypeKind declarationKind)
  Language.PureScript.Externs.EDTypeSynonym name arguments type' ->
    Language.PureScript.Externs.EDTypeSynonym
      name
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) arguments)
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')
  Language.PureScript.Externs.EDValue name type' ->
    Language.PureScript.Externs.EDValue
      name
      (Language.PureScript.AST.SourcePos.nullSourceAnn <$ type')

makeSignatureExternsFile ::
  Language.PureScript.Externs.ExternsFile ->
  Language.PureScript.Externs.ExternsFile
makeSignatureExternsFile externsFile = case externsFile of
  Language.PureScript.Externs.ExternsFile version' moduleName exports imports fixities typeFixities declarations _sourceSpan ->
    Language.PureScript.Externs.ExternsFile
      version'
      moduleName
      (fmap makeSignatureDeclarationRef exports)
      (fmap makeSignatureExternsImport imports)
      fixities
      typeFixities
      (fmap makeSignatureExternsDeclaration declarations)
      Language.PureScript.AST.SourcePos.nullSourceSpan

makeSignatureExternsImport ::
  Language.PureScript.Externs.ExternsImport ->
  Language.PureScript.Externs.ExternsImport
makeSignatureExternsImport externsImport = case externsImport of
  Language.PureScript.Externs.ExternsImport moduleName importDeclarationType importedAs ->
    Language.PureScript.Externs.ExternsImport
      moduleName
      (makeSignatureImportDeclarationType importDeclarationType)
      importedAs

makeSignatureImportDeclarationType ::
  Language.PureScript.AST.Declarations.ImportDeclarationType ->
  Language.PureScript.AST.Declarations.ImportDeclarationType
makeSignatureImportDeclarationType importDeclarationType = case importDeclarationType of
  Language.PureScript.AST.Declarations.Explicit imports ->
    Language.PureScript.AST.Declarations.Explicit
      (fmap makeSignatureDeclarationRef imports)
  Language.PureScript.AST.Declarations.Hiding imports ->
    Language.PureScript.AST.Declarations.Hiding
      (fmap makeSignatureDeclarationRef imports)
  Language.PureScript.AST.Declarations.Implicit ->
    Language.PureScript.AST.Declarations.Implicit

makeSignatureTypeKind ::
  Language.PureScript.Environment.TypeKind ->
  Language.PureScript.Environment.TypeKind
makeSignatureTypeKind typeKind = case typeKind of
  Language.PureScript.Environment.DataType typeArguments constructors ->
    Language.PureScript.Environment.DataType
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) typeArguments)
      ((fmap . fmap . fmap) (Language.PureScript.AST.SourcePos.nullSourceAnn <$) constructors)
  Language.PureScript.Environment.TypeSynonym ->
    Language.PureScript.Environment.TypeSynonym
  Language.PureScript.Environment.ExternData ->
    Language.PureScript.Environment.ExternData
  Language.PureScript.Environment.LocalTypeVariable ->
    Language.PureScript.Environment.LocalTypeVariable
  Language.PureScript.Environment.ScopedTypeVar ->
    Language.PureScript.Environment.ScopedTypeVar
