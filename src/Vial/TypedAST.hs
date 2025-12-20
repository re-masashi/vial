{-# OPTIONS_GHC -Wno-missing-methods #-}

module Vial.TypedAST where

import Data.Bifunctor
import Vial.AST (BinOp, Ident, Literal, MacroBody, Metadata, UnOp)
import Vial.AST qualified as AST

-- Inferred types (System Fω)
data InferredType where
  InferredType :: {inferredKind :: InferredTypeKind} -> InferredType
  deriving (Show, Eq)

data InferredTypeKind where
  ITyVar :: Ident -> InferredTypeKind
  ITyCon :: Ident -> InferredTypeKind
  ITyApp :: InferredType -> InferredType -> InferredTypeKind
  ITyFunc :: [InferredType] -> InferredType -> InferredTypeKind
  ITyForall :: [Ident] -> InferredType -> InferredTypeKind
  ITyOption :: InferredType -> InferredTypeKind
  ITyRecord :: [(Ident, InferredType)] -> InferredTypeKind
  deriving (Show, Eq)

data TypedParam where
  TypedParam ::
    { tpMeta :: Metadata,
      tpName :: Ident,
      tpType :: InferredType,
      tpMut :: Bool
    } ->
    TypedParam
  deriving (Show, Eq)

data TypedField where
  TypedField ::
    { tfMeta :: Metadata,
      tfName :: Ident,
      tfType :: InferredType
    } ->
    TypedField
  deriving (Show, Eq)

data TypedVariant where
  TypedVariant :: {tvMeta :: Metadata, tvKind :: TypedVariantKind} -> TypedVariant
  deriving (Show, Eq)

data TypedVariantKind where
  TVSimple :: Ident -> TypedVariantKind
  TVTuple :: Ident -> [InferredType] -> (Maybe InferredType) -> TypedVariantKind
  TVStruct :: Ident -> [TypedField] -> (Maybe InferredType) -> TypedVariantKind
  deriving (Show, Eq)

data TypedExpr where
  TypedExpr ::
    { texprMeta :: Metadata,
      texprType :: InferredType,
      texprKind :: TypedExprKind
    } ->
    TypedExpr
  deriving (Show, Eq)

data TypedExprKind where
  TELit :: Literal -> TypedExprKind
  TEVar :: Ident -> TypedExprKind
  TEBinOp :: BinOp -> TypedExpr -> TypedExpr -> TypedExprKind
  TEUnOp :: UnOp -> TypedExpr -> TypedExprKind
  TEIf :: TypedExpr -> TypedExpr -> TypedExpr -> TypedExprKind
  TEMatch :: TypedExpr -> [TypedMatchArm] -> TypedExprKind
  TEBlock :: [TypedExpr] -> TypedExprKind
  TECall :: TypedExpr -> [TypedExpr] -> TypedExprKind
  TEField :: TypedExpr -> Ident -> TypedExprKind
  TEMethod :: TypedExpr -> Ident -> [TypedExpr] -> TypedExprKind
  TESpawn :: Ident -> [TypedExpr] -> TypedExprKind
  TESend :: TypedExpr -> TypedExpr -> TypedExprKind
  TEReceive :: [TypedMatchArm] -> TypedExprKind
  TEMacro :: Ident -> MacroBody -> TypedExprKind
  TELet :: Ident -> InferredType -> TypedExpr -> Bool -> TypedExprKind
  TEAssign :: TypedExpr -> TypedExpr -> TypedExprKind
  TEFor :: Ident -> TypedExpr -> TypedExpr -> TypedExprKind
  TEDefer :: TypedExpr -> TypedExprKind
  TEMove :: TypedExpr -> TypedExprKind
  TERefMut :: TypedExpr -> TypedExprKind
  TEAnonRecord :: [(Ident, TypedExpr)] -> TypedExprKind
  TECast :: TypedExpr -> InferredType -> TypedExprKind
  deriving (Show, Eq)

data TypedPattern where
  TypedPattern ::
    { tpatMeta :: Metadata,
      tpatType :: InferredType,
      tpatKind :: TypedPatternKind
    } ->
    TypedPattern
  deriving (Show, Eq)

data TypedPatternKind where
  TPVar :: Ident -> TypedPatternKind
  TPLit :: Literal -> TypedPatternKind
  TPCon :: Ident -> [TypedPattern] -> TypedPatternKind
  TPStruct :: Ident -> [(Ident, TypedPattern)] -> TypedPatternKind
  TPWildcard :: TypedPatternKind
  deriving (Show, Eq)

data TypedMatchArm where
  TypedMatchArm ::
    { ttarmMeta :: Metadata,
      ttarmPat :: TypedPattern,
      ttarmExpr :: TypedExpr
    } ->
    TypedMatchArm
  deriving (Show, Eq)

data TypedDecl where
  TypedDecl ::
    { tdeclMeta :: Metadata,
      tdeclKind :: TypedDeclKind
    } ->
    TypedDecl
  deriving (Show, Eq)

data TypedDeclKind where
  TDFunc ::
    Ident ->
    [AST.TypeParam] ->
    [TypedParam] ->
    InferredType ->
    TypedExpr ->
    TypedDeclKind
  TDStruct :: Ident -> [AST.TypeParam] -> [TypedField] -> TypedDeclKind
  TDEnum :: Ident -> [AST.TypeParam] -> [TypedVariant] -> TypedDeclKind
  TDTrait :: Ident -> [AST.TypeParam] -> [TypedTraitItem] -> TypedDeclKind
  TDImpl :: Ident -> [AST.TypeParam] -> InferredType -> [TypedImplItem] -> TypedDeclKind
  TDActor :: Ident -> [TypedActorItem] -> TypedDeclKind
  deriving (Show, Eq)

data TypedTraitItem where
  TypedTraitItem ::
    {ttiMeta :: Metadata, ttiKind :: TypedTraitItemKind} ->
    TypedTraitItem
  deriving (Show, Eq)

data TypedTraitItemKind where
  TTFunc ::
    Ident ->
    [AST.TypeParam] ->
    [TypedParam] ->
    InferredType ->
    (Maybe TypedExpr) ->
    TypedTraitItemKind
  TTType :: Ident -> InferredType -> TypedTraitItemKind
  deriving (Show, Eq)

data TypedImplItem where
  TypedImplItem ::
    {tiMeta :: Metadata, tiKind :: TypedImplItemKind} ->
    TypedImplItem
  deriving (Show, Eq)

data TypedImplItemKind where
  TIFunc ::
    Ident ->
    [AST.TypeParam] ->
    [TypedParam] ->
    InferredType ->
    TypedExpr ->
    TypedImplItemKind
  TIType :: Ident -> InferredType -> TypedImplItemKind
  deriving (Show, Eq)

data TypedActorItem where
  TypedActorItem ::
    {taiMeta :: Metadata, taiKind :: TypedActorItemKind} ->
    TypedActorItem
  deriving (Show, Eq)

data TypedActorItemKind where
  TALet :: Ident -> InferredType -> TypedExpr -> Bool -> TypedActorItemKind
  TABehavior :: Ident -> [TypedParam] -> TypedExpr -> TypedActorItemKind
  TAReceive :: [TypedMatchArm] -> TypedActorItemKind
  deriving (Show, Eq)

data TypedProgram where
  TypedProgram ::
    { tpImports :: [AST.Import],
      tpDecls :: [TypedDecl]
    } ->
    TypedProgram
  deriving (Show, Eq)

class TypedVisitor r where
  visitInferredType :: InferredType -> r
  visitInferredTypeKind :: InferredTypeKind -> r
  visitTypedParam :: TypedParam -> r
  visitTypedField :: TypedField -> r
  visitTypedVariant :: TypedVariant -> r
  visitTypedVariantKind :: TypedVariantKind -> r
  visitTypedExpr :: TypedExpr -> r
  visitTypedExprKind :: TypedExprKind -> r
  visitTypedPattern :: TypedPattern -> r
  visitTypedPatternKind :: TypedPatternKind -> r
  visitTypedMatchArm :: TypedMatchArm -> r
  visitTypedDecl :: TypedDecl -> r
  visitTypedDeclKind :: TypedDeclKind -> r
  visitTypedTraitItem :: TypedTraitItem -> r
  visitTypedTraitItemKind :: TypedTraitItemKind -> r
  visitTypedImplItem :: TypedImplItem -> r
  visitTypedImplItemKind :: TypedImplItemKind -> r
  visitTypedActorItem :: TypedActorItem -> r
  visitTypedActorItemKind :: TypedActorItemKind -> r
  visitTypedProgram :: TypedProgram -> r

newtype TypedIdentity a = TypedIdentity {runTypedIdentity :: a}

instance TypedVisitor (TypedIdentity InferredType) where
  visitInferredType (InferredType kind) = TypedIdentity (InferredType (runTypedIdentity (visitInferredTypeKind kind)))

instance TypedVisitor (TypedIdentity InferredTypeKind) where
  visitInferredTypeKind (ITyVar i) = TypedIdentity (ITyVar i)
  visitInferredTypeKind (ITyCon i) = TypedIdentity (ITyCon i)
  visitInferredTypeKind (ITyApp t1 t2) = TypedIdentity (ITyApp (runTypedIdentity (visitInferredType t1)) (runTypedIdentity (visitInferredType t2)))
  visitInferredTypeKind (ITyFunc ts t) = TypedIdentity (ITyFunc (map (runTypedIdentity . visitInferredType) ts) (runTypedIdentity (visitInferredType t)))
  visitInferredTypeKind (ITyForall is t) = TypedIdentity (ITyForall is (runTypedIdentity (visitInferredType t)))
  visitInferredTypeKind (ITyOption t) = TypedIdentity (ITyOption (runTypedIdentity (visitInferredType t)))
  visitInferredTypeKind (ITyRecord fields) = TypedIdentity (ITyRecord (map (\(i, t) -> (i, runTypedIdentity (visitInferredType t))) fields))

instance TypedVisitor (TypedIdentity TypedParam) where
  visitTypedParam (TypedParam meta name typ mut) = TypedIdentity (TypedParam meta name (runTypedIdentity (visitInferredType typ)) mut)

instance TypedVisitor (TypedIdentity TypedField) where
  visitTypedField (TypedField meta name typ) = TypedIdentity (TypedField meta name (runTypedIdentity (visitInferredType typ)))

instance TypedVisitor (TypedIdentity TypedVariant) where
  visitTypedVariant (TypedVariant meta kind) = TypedIdentity (TypedVariant meta (runTypedIdentity (visitTypedVariantKind kind)))

instance TypedVisitor (TypedIdentity TypedVariantKind) where
  visitTypedVariantKind (TVSimple i) = TypedIdentity (TVSimple i)
  visitTypedVariantKind (TVTuple i ts mt) = TypedIdentity (TVTuple i (map (runTypedIdentity . visitInferredType) ts) (fmap (runTypedIdentity . visitInferredType) mt))
  visitTypedVariantKind (TVStruct i fields mt) = TypedIdentity (TVStruct i (map (runTypedIdentity . visitTypedField) fields) (fmap (runTypedIdentity . visitInferredType) mt))

instance TypedVisitor (TypedIdentity TypedExpr) where
  visitTypedExpr (TypedExpr meta typ kind) =
    TypedIdentity (TypedExpr meta (runTypedIdentity (visitInferredType typ)) (runTypedIdentity (visitTypedExprKind kind)))

instance TypedVisitor (TypedIdentity TypedExprKind) where
  visitTypedExprKind (TELit l) = TypedIdentity (TELit l)
  visitTypedExprKind (TEVar i) = TypedIdentity (TEVar i)
  visitTypedExprKind (TEBinOp op e1 e2) =
    TypedIdentity (TEBinOp op (runTypedIdentity (visitTypedExpr e1)) (runTypedIdentity (visitTypedExpr e2)))
  visitTypedExprKind (TEUnOp op e) =
    TypedIdentity (TEUnOp op (runTypedIdentity (visitTypedExpr e)))
  visitTypedExprKind (TEIf c t f) =
    TypedIdentity (TEIf (runTypedIdentity (visitTypedExpr c)) (runTypedIdentity (visitTypedExpr t)) (runTypedIdentity (visitTypedExpr f)))
  visitTypedExprKind (TEMatch e arms) =
    TypedIdentity (TEMatch (runTypedIdentity (visitTypedExpr e)) (map (runTypedIdentity . visitTypedMatchArm) arms))
  visitTypedExprKind (TEBlock es) =
    TypedIdentity (TEBlock (map (runTypedIdentity . visitTypedExpr) es))
  visitTypedExprKind (TECall e es) =
    TypedIdentity (TECall (runTypedIdentity (visitTypedExpr e)) (map (runTypedIdentity . visitTypedExpr) es))
  visitTypedExprKind (TEField e i) =
    TypedIdentity (TEField (runTypedIdentity (visitTypedExpr e)) i)
  visitTypedExprKind (TEMethod e i es) =
    TypedIdentity (TEMethod (runTypedIdentity (visitTypedExpr e)) i (map (runTypedIdentity . visitTypedExpr) es))
  visitTypedExprKind (TESpawn i es) =
    TypedIdentity (TESpawn i (map (runTypedIdentity . visitTypedExpr) es))
  visitTypedExprKind (TESend e1 e2) =
    TypedIdentity (TESend (runTypedIdentity (visitTypedExpr e1)) (runTypedIdentity (visitTypedExpr e2)))
  visitTypedExprKind (TEReceive arms) =
    TypedIdentity (TEReceive (map (runTypedIdentity . visitTypedMatchArm) arms))
  visitTypedExprKind (TEMacro i body) =
    TypedIdentity (TEMacro i body)
  visitTypedExprKind (TELet i t e b) =
    TypedIdentity (TELet i (runTypedIdentity (visitInferredType t)) (runTypedIdentity (visitTypedExpr e)) b)
  visitTypedExprKind (TEAssign e1 e2) =
    TypedIdentity (TEAssign (runTypedIdentity (visitTypedExpr e1)) (runTypedIdentity (visitTypedExpr e2)))
  visitTypedExprKind (TEFor i e1 e2) =
    TypedIdentity (TEFor i (runTypedIdentity (visitTypedExpr e1)) (runTypedIdentity (visitTypedExpr e2)))
  visitTypedExprKind (TEDefer e) =
    TypedIdentity (TEDefer (runTypedIdentity (visitTypedExpr e)))
  visitTypedExprKind (TEMove e) =
    TypedIdentity (TEMove (runTypedIdentity (visitTypedExpr e)))
  visitTypedExprKind (TERefMut e) =
    TypedIdentity (TERefMut (runTypedIdentity (visitTypedExpr e)))
  visitTypedExprKind (TEAnonRecord fields) =
    TypedIdentity (TEAnonRecord (map (\(i, e) -> (i, runTypedIdentity (visitTypedExpr e))) fields))
  visitTypedExprKind (TECast e t) =
    TypedIdentity (TECast (runTypedIdentity (visitTypedExpr e)) (runTypedIdentity (visitInferredType t)))

instance TypedVisitor (TypedIdentity TypedPattern) where
  visitTypedPattern (TypedPattern meta typ kind) =
    TypedIdentity (TypedPattern meta (runTypedIdentity (visitInferredType typ)) (runTypedIdentity (visitTypedPatternKind kind)))

instance TypedVisitor (TypedIdentity TypedPatternKind) where
  visitTypedPatternKind (TPVar i) = TypedIdentity (TPVar i)
  visitTypedPatternKind (TPLit l) = TypedIdentity (TPLit l)
  visitTypedPatternKind (TPCon i ps) =
    TypedIdentity (TPCon i (map (runTypedIdentity . visitTypedPattern) ps))
  visitTypedPatternKind (TPStruct i fields) =
    TypedIdentity (TPStruct i (map (\(j, p) -> (j, runTypedIdentity (visitTypedPattern p))) fields))
  visitTypedPatternKind TPWildcard = TypedIdentity TPWildcard

instance TypedVisitor (TypedIdentity TypedMatchArm) where
  visitTypedMatchArm (TypedMatchArm meta pat expr) =
    TypedIdentity (TypedMatchArm meta (runTypedIdentity (visitTypedPattern pat)) (runTypedIdentity (visitTypedExpr expr)))

instance TypedVisitor (TypedIdentity TypedDecl) where
  visitTypedDecl (TypedDecl meta kind) =
    TypedIdentity (TypedDecl meta (runTypedIdentity (visitTypedDeclKind kind)))

instance TypedVisitor (TypedIdentity TypedDeclKind) where
  visitTypedDeclKind (TDFunc i tps ps t e) =
    TypedIdentity (TDFunc i tps (map (runTypedIdentity . visitTypedParam) ps) (runTypedIdentity (visitInferredType t)) (runTypedIdentity (visitTypedExpr e)))
  visitTypedDeclKind (TDStruct i tps fields) =
    TypedIdentity (TDStruct i tps (map (runTypedIdentity . visitTypedField) fields))
  visitTypedDeclKind (TDEnum i tps vars) =
    TypedIdentity (TDEnum i tps (map (runTypedIdentity . visitTypedVariant) vars))
  visitTypedDeclKind (TDTrait i tps items) =
    TypedIdentity (TDTrait i tps (map (runTypedIdentity . visitTypedTraitItem) items))
  visitTypedDeclKind (TDImpl i tps t items) =
    TypedIdentity (TDImpl i tps (runTypedIdentity (visitInferredType t)) (map (runTypedIdentity . visitTypedImplItem) items))
  visitTypedDeclKind (TDActor i items) =
    TypedIdentity (TDActor i (map (runTypedIdentity . visitTypedActorItem) items))

instance TypedVisitor (TypedIdentity TypedTraitItem) where
  visitTypedTraitItem (TypedTraitItem meta kind) =
    TypedIdentity (TypedTraitItem meta (runTypedIdentity (visitTypedTraitItemKind kind)))

instance TypedVisitor (TypedIdentity TypedTraitItemKind) where
  visitTypedTraitItemKind (TTFunc i tps ps t me) =
    TypedIdentity (TTFunc i tps (map (runTypedIdentity . visitTypedParam) ps) (runTypedIdentity (visitInferredType t)) (fmap (runTypedIdentity . visitTypedExpr) me))
  visitTypedTraitItemKind (TTType i t) =
    TypedIdentity (TTType i (runTypedIdentity (visitInferredType t)))

instance TypedVisitor (TypedIdentity TypedImplItem) where
  visitTypedImplItem (TypedImplItem meta kind) =
    TypedIdentity (TypedImplItem meta (runTypedIdentity (visitTypedImplItemKind kind)))

instance TypedVisitor (TypedIdentity TypedImplItemKind) where
  visitTypedImplItemKind (TIFunc i tps ps t e) =
    TypedIdentity (TIFunc i tps (map (runTypedIdentity . visitTypedParam) ps) (runTypedIdentity (visitInferredType t)) (runTypedIdentity (visitTypedExpr e)))
  visitTypedImplItemKind (TIType i t) =
    TypedIdentity (TIType i (runTypedIdentity (visitInferredType t)))

instance TypedVisitor (TypedIdentity TypedActorItem) where
  visitTypedActorItem (TypedActorItem meta kind) =
    TypedIdentity (TypedActorItem meta (runTypedIdentity (visitTypedActorItemKind kind)))

instance TypedVisitor (TypedIdentity TypedActorItemKind) where
  visitTypedActorItemKind (TALet i t e b) =
    TypedIdentity (TALet i (runTypedIdentity (visitInferredType t)) (runTypedIdentity (visitTypedExpr e)) b)
  visitTypedActorItemKind (TABehavior i ps e) =
    TypedIdentity (TABehavior i (map (runTypedIdentity . visitTypedParam) ps) (runTypedIdentity (visitTypedExpr e)))
  visitTypedActorItemKind (TAReceive arms) =
    TypedIdentity (TAReceive (map (runTypedIdentity . visitTypedMatchArm) arms))

instance TypedVisitor (TypedIdentity TypedProgram) where
  visitTypedProgram (TypedProgram imps decls) =
    TypedIdentity (TypedProgram imps (map (runTypedIdentity . visitTypedDecl) decls))

-- | Get the type of a typed expression
typeOf :: TypedExpr -> InferredType
typeOf (TypedExpr _ t _) = t

typeOfPattern :: TypedPattern -> InferredType
typeOfPattern (TypedPattern _ t _) = t

-- _Untypes your typed expression_
untypeType :: InferredType -> AST.Type
untypeType (InferredType kind) = AST.Type dummyMeta (untypeTypeKind kind)
  where
    dummyMeta = AST.Metadata (AST.Span 0 0) "<inferred>" []
    untypeTypeKind :: InferredTypeKind -> AST.TypeKind
    untypeTypeKind (ITyVar i) = AST.TyVar i
    untypeTypeKind (ITyCon i) = AST.TyCon i []
    untypeTypeKind (ITyApp t1 t2) = AST.TyApp (untypeType t1) (untypeType t2)
    untypeTypeKind (ITyFunc ts t) = AST.TyFunc (map untypeType ts) (untypeType t)
    untypeTypeKind (ITyForall _ t) = AST.typeKind (untypeType t)  -- ignore forall
    untypeTypeKind (ITyOption t) = AST.TyOption (untypeType t)
    untypeTypeKind (ITyRecord fields) = AST.TyRecord (map (\(i, t) -> (i, untypeType t)) fields) Nothing

untypeTypedParam :: TypedParam -> AST.Param
untypeTypedParam (TypedParam meta name typ mut) = AST.Param meta name (untypeType typ) mut

untypeTypedField :: TypedField -> AST.Field
untypeTypedField (TypedField meta name typ) = AST.Field meta name (untypeType typ)

untypeTypedVariant :: TypedVariant -> AST.Variant
untypeTypedVariant (TypedVariant meta kind) = AST.Variant meta (untypeTypedVariantKind kind)
  where
    untypeTypedVariantKind :: TypedVariantKind -> AST.VariantKind
    untypeTypedVariantKind (TVSimple i) = AST.VSimple i
    untypeTypedVariantKind (TVTuple i ts mt) = AST.VTuple i (map untypeType ts) (fmap untypeType mt)
    untypeTypedVariantKind (TVStruct i fields mt) = AST.VStruct i (map untypeTypedField fields) (fmap untypeType mt)

untypeExpr :: TypedExpr -> AST.Expr
untypeExpr (TypedExpr meta _ kind) = AST.Expr meta (untypeExprKind kind)
  where
    untypeExprKind :: TypedExprKind -> AST.ExprKind
    untypeExprKind (TELit l) = AST.ELit l
    untypeExprKind (TEVar i) = AST.EVar i
    untypeExprKind (TEBinOp op e1 e2) = AST.EBinOp op (untypeExpr e1) (untypeExpr e2)
    untypeExprKind (TEUnOp op e) = AST.EUnOp op (untypeExpr e)
    untypeExprKind (TEIf c t f) = AST.EIf (untypeExpr c) (untypeExpr t) (untypeExpr f)
    untypeExprKind (TEMatch e arms) = AST.EMatch (untypeExpr e) (map untypeMatchArm arms)
    untypeExprKind (TEBlock es) = AST.EBlock (map untypeExpr es)
    untypeExprKind (TECall e es) = AST.ECall (untypeExpr e) (map untypeExpr es)
    untypeExprKind (TEField e i) = AST.EField (untypeExpr e) i
    untypeExprKind (TEMethod e i es) = AST.EMethod (untypeExpr e) i (map untypeExpr es)
    untypeExprKind (TESpawn i es) = AST.ESpawn i (map untypeExpr es)
    untypeExprKind (TESend e1 e2) = AST.ESend (untypeExpr e1) (untypeExpr e2)
    untypeExprKind (TEReceive arms) = AST.EReceive (map untypeMatchArm arms)
    untypeExprKind (TEMacro i body) = AST.EMacro i body
    untypeExprKind (TELet i t e b) = AST.ELet i (Just (untypeType t)) (untypeExpr e) b
    untypeExprKind (TEAssign e1 e2) = AST.EAssign (untypeExpr e1) (untypeExpr e2)
    untypeExprKind (TEFor i e1 e2) = AST.EFor i (untypeExpr e1) (untypeExpr e2)
    untypeExprKind (TEDefer e) = AST.EDefer (untypeExpr e)
    untypeExprKind (TEMove e) = AST.EMove (untypeExpr e)
    untypeExprKind (TERefMut e) = AST.ERefMut (untypeExpr e)
    untypeExprKind (TEAnonRecord fields) = AST.EAnonRecord (map (Data.Bifunctor.second untypeExpr) fields)
    untypeExprKind (TECast e t) = AST.ECast (untypeExpr e) (untypeType t)

    untypeMatchArm :: TypedMatchArm -> AST.MatchArm
    untypeMatchArm (TypedMatchArm armMeta pat expr) = AST.MatchArm armMeta (untypePattern pat) (untypeExpr expr)

-- | Extract the untyped pattern from a typed pattern
untypePattern :: TypedPattern -> AST.Pattern
untypePattern (TypedPattern meta _ kind) = AST.Pattern meta (untypePatternKind kind)
  where
    untypePatternKind :: TypedPatternKind -> AST.PatternKind
    untypePatternKind (TPVar i) = AST.PVar i
    untypePatternKind (TPLit l) = AST.PLit l
    untypePatternKind (TPCon i ps) = AST.PCon i (map untypePattern ps)
    untypePatternKind (TPStruct i fields) = AST.PStruct i (map (Data.Bifunctor.second untypePattern) fields)
    untypePatternKind TPWildcard = AST.PWildcard

-- | Extract the untyped declaration from a typed declaration
untypeDecl :: TypedDecl -> AST.Decl
untypeDecl (TypedDecl meta kind) = AST.Decl meta (untypeDeclKind kind)
  where
    untypeDeclKind :: TypedDeclKind -> AST.DeclKind
    untypeDeclKind (TDFunc i tps ps t e) = AST.DFunc i tps (map untypeTypedParam ps) (Just (untypeType t)) (untypeExpr e)
    untypeDeclKind (TDStruct i tps fields) = AST.DStruct i tps (map untypeTypedField fields)
    untypeDeclKind (TDEnum i tps vars) = AST.DEnum i tps (map untypeTypedVariant vars)
    untypeDeclKind (TDTrait i tps items) = AST.DTrait i tps (map untypeTraitItem items)
    untypeDeclKind (TDImpl i tps t items) = AST.DImpl i tps (untypeType t) (map untypeImplItem items)
    untypeDeclKind (TDActor i items) = AST.DActor i (map untypeActorItem items)

    untypeTraitItem :: TypedTraitItem -> AST.TraitItem
    untypeTraitItem (TypedTraitItem traitItemMeta traitItemKind) = AST.TraitItem traitItemMeta (untypeTraitItemKind traitItemKind)

    untypeTraitItemKind :: TypedTraitItemKind -> AST.TraitItemKind
    untypeTraitItemKind (TTFunc i tps ps t me) = AST.TFunc i tps (map untypeTypedParam ps) (Just (untypeType t)) (fmap untypeExpr me)
    untypeTraitItemKind (TTType i _) = AST.TType i

    untypeImplItem :: TypedImplItem -> AST.ImplItem
    untypeImplItem (TypedImplItem implItemMeta implItemKind) = AST.ImplItem implItemMeta (untypeImplItemKind implItemKind)

    untypeImplItemKind :: TypedImplItemKind -> AST.ImplItemKind
    untypeImplItemKind (TIFunc i tps ps t e) = AST.IFunc i tps (map untypeTypedParam ps) (Just (untypeType t)) (untypeExpr e)
    untypeImplItemKind (TIType i t) = AST.IType i (untypeType t)

    untypeActorItem :: TypedActorItem -> AST.ActorItem
    untypeActorItem (TypedActorItem actorItemMeta actorItemKind) = AST.ActorItem actorItemMeta (untypeActorItemKind actorItemKind)

    untypeActorItemKind :: TypedActorItemKind -> AST.ActorItemKind
    untypeActorItemKind (TALet i t e b) = AST.ALet i (Just (untypeType t)) (untypeExpr e) b
    untypeActorItemKind (TABehavior i ps e) = AST.ABehavior i (map untypeTypedParam ps) (untypeExpr e)
    untypeActorItemKind (TAReceive arms) = AST.AReceive (map untypeMatchArm arms)

    untypeMatchArm :: TypedMatchArm -> AST.MatchArm
    untypeMatchArm (TypedMatchArm matchArmMeta pat expr) = AST.MatchArm matchArmMeta (untypePattern pat) (untypeExpr expr)

-- | Extract the untyped program from a typed program
untypeProgram :: TypedProgram -> AST.Program
untypeProgram (TypedProgram imps decls) = AST.Program imps (map untypeDecl decls)
