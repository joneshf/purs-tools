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
import qualified "purescript" Language.PureScript.Names
import "rio" RIO hiding (error)
import qualified "rio" RIO.Set

codegen ::
  Language.PureScript.Externs.ExternsFile ->
  FilePath ->
  Language.PureScript.Make.Monad.Make ()
codegen externsFile' outputExternsFile = do
  let externsFile :: Language.PureScript.Externs.ExternsFile
      externsFile = makeSignatureExternsFile externsFile'
  Language.PureScript.Make.Monad.writeCborFile outputExternsFile externsFile

-- | We find any modules that are being re-exported.
-- | We have to make sure we find both the module where something is actually declared and the module we imported to get the declaration.
-- | These two can be different in the case that we're re-exporting something that's already been re-exported.
findRexport ::
  Language.PureScript.AST.Declarations.DeclarationRef ->
  Set Language.PureScript.Names.ModuleName
findRexport declarationRef = case declarationRef of
  Language.PureScript.AST.Declarations.KindRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.ModuleRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.ReExportRef _ exportSource _ ->
    RIO.Set.singleton (Language.PureScript.AST.Declarations.exportSourceDefinedIn exportSource)
      <> foldMap RIO.Set.singleton (Language.PureScript.AST.Declarations.exportSourceImportedFrom exportSource)
  Language.PureScript.AST.Declarations.TypeClassRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.TypeInstanceRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.TypeOpRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.TypeRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.ValueOpRef {} -> RIO.Set.empty
  Language.PureScript.AST.Declarations.ValueRef {} -> RIO.Set.empty

findRexports ::
  [Language.PureScript.AST.Declarations.DeclarationRef] ->
  Set Language.PureScript.Names.ModuleName
findRexports = foldMap findRexport

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
      (mapMaybe (makeSignatureExternsImport (findRexports exports)) imports)
      fixities
      typeFixities
      (fmap makeSignatureExternsDeclaration declarations)
      Language.PureScript.AST.SourcePos.nullSourceSpan

makeSignatureExternsImport ::
  Set Language.PureScript.Names.ModuleName ->
  Language.PureScript.Externs.ExternsImport ->
  Maybe Language.PureScript.Externs.ExternsImport
makeSignatureExternsImport rexports externsImport = case externsImport of
  Language.PureScript.Externs.ExternsImport moduleName importDeclarationType importedAs
    | RIO.Set.notMember moduleName rexports -> Nothing
    | otherwise ->
      Just
        ( Language.PureScript.Externs.ExternsImport
            moduleName
            (makeSignatureImportDeclarationType importDeclarationType)
            importedAs
        )

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
