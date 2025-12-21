{-# OPTIONS_GHC -Wno-missing-methods #-}

module Vial.TypedAST where

import Vial.AST (BinOp, Ident, Literal, MacroBody, Metadata, UnOp)
import Vial.AST qualified as AST

-- Inferred types (System Fω)
data InferredType where
  InferredType :: {inferredKind :: InferredTypeKind} -> InferredType
  deriving (Show, Eq)

-- | Kinds in System F-omega: * represents proper types, and k1 -> k2 represents type constructors
data Kind where
  -- | The kind of proper types (e.g., Int, Bool)
  Star :: Kind
  -- | The kind of type constructors (e.g., List -> *, Functor -> (* -> *) -> *)
  KArrow :: Kind -> Kind -> Kind
  deriving (Show, Eq)

data InferredTypeKind where
  -- | Type variable (e.g., 'a in forall a. a -> a)
  ITyVar :: Ident -> InferredTypeKind
  -- | Type constructor (e.g., List, Maybe)
  ITyCon :: Ident -> InferredTypeKind
  -- | Type application (e.g., List Int, Maybe String)
  ITyApp :: InferredType -> InferredType -> InferredTypeKind
  -- | Function type (e.g., Int -> Bool)
  ITyFunc :: [InferredType] -> InferredType -> InferredTypeKind
  -- | Universal quantification with kind annotations (e.g., forall a:*. a -> a)
  ITyForall :: [(Ident, Kind)] -> InferredType -> InferredTypeKind
  -- | Option type (e.g., Option Int)
  ITyOption :: InferredType -> InferredTypeKind
  -- | Record type (e.g., {name: String, age: Int})
  ITyRecord :: [(Ident, InferredType)] -> InferredTypeKind
  -- | Type-level lambda abstraction for type operators (e.g., \a -> List a)
  ITyLam :: Ident -> InferredType -> InferredTypeKind
  -- | Dynamic trait object (e.g., dyn Show)
  ITyDyn :: Ident -> InferredTypeKind
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
  TELambda :: [TypedParam] -> TypedExpr -> TypedExprKind
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
  TEVariant :: Ident -> Ident -> [TypedExpr] -> TypedExprKind
  TEQuestion :: TypedExpr -> TypedExprKind
  TEComptime :: TypedExpr -> TypedExprKind
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
  TDConst :: Ident -> InferredType -> TypedExpr -> TypedDeclKind
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

class (Monad m) => TypedVisitor m where
  visitInferredType :: InferredType -> m AST.Type
  visitInferredTypeKind :: InferredTypeKind -> m AST.TypeKind
  visitKind :: Kind -> m AST.Kind
  visitTypedParam :: TypedParam -> m AST.Param
  visitTypedField :: TypedField -> m AST.Field
  visitTypedVariant :: TypedVariant -> m AST.Variant
  visitTypedVariantKind :: TypedVariantKind -> m AST.VariantKind
  visitTypedExpr :: TypedExpr -> m AST.Expr
  visitTypedExprKind :: TypedExprKind -> m AST.ExprKind
  visitTypedPattern :: TypedPattern -> m AST.Pattern
  visitTypedPatternKind :: TypedPatternKind -> m AST.PatternKind
  visitTypedMatchArm :: TypedMatchArm -> m AST.MatchArm
  visitTypedDecl :: TypedDecl -> m AST.Decl
  visitTypedDeclKind :: TypedDeclKind -> m AST.DeclKind
  visitTypedTraitItem :: TypedTraitItem -> m AST.TraitItem
  visitTypedTraitItemKind :: TypedTraitItemKind -> m AST.TraitItemKind
  visitTypedImplItem :: TypedImplItem -> m AST.ImplItem
  visitTypedImplItemKind :: TypedImplItemKind -> m AST.ImplItemKind
  visitTypedActorItem :: TypedActorItem -> m AST.ActorItem
  visitTypedActorItemKind :: TypedActorItemKind -> m AST.ActorItemKind
  visitTypedProgram :: TypedProgram -> m AST.Program

newtype Untype a = Untype {runUntype :: a}

instance Functor Untype where
  fmap f (Untype a) = Untype (f a)

instance Applicative Untype where
  pure = Untype
  Untype f <*> Untype a = Untype (f a)

instance Monad Untype where
  return = pure
  Untype a >>= f = f a

instance TypedVisitor Untype where
  visitInferredType (InferredType kind) = AST.Type dummyMeta <$> visitInferredTypeKind kind
    where
      dummyMeta = AST.Metadata (AST.Span 0 0) "<inferred>" []

  visitInferredTypeKind (ITyVar i) = pure (AST.TyVar i)
  visitInferredTypeKind (ITyCon i) = pure (AST.TyCon i [])
  visitInferredTypeKind (ITyApp t1 t2) = AST.TyApp <$> visitInferredType t1 <*> visitInferredType t2
  visitInferredTypeKind (ITyFunc ts t) = AST.TyFunc <$> traverse visitInferredType ts <*> visitInferredType t
  visitInferredTypeKind (ITyForall _ t) = AST.typeKind <$> visitInferredType t -- ignore forall and kinds for untyping
  visitInferredTypeKind (ITyOption t) = AST.TyOption <$> visitInferredType t
  visitInferredTypeKind (ITyRecord fields) = AST.TyRecord <$> traverse (\(i, t) -> (,) i <$> visitInferredType t) fields <*> pure Nothing
  visitInferredTypeKind (ITyLam _ t) = AST.typeKind <$> visitInferredType t -- ignore type lambda for untyping
  visitInferredTypeKind (ITyDyn i) = pure (AST.TyDyn i)

  visitKind Star = pure AST.KStar
  visitKind (KArrow k1 k2) = AST.KArr <$> visitKind k1 <*> visitKind k2

  visitTypedParam (TypedParam meta name typ mut) = AST.Param meta name <$> visitInferredType typ <*> pure mut

  visitTypedField (TypedField meta name typ) = AST.Field meta name <$> visitInferredType typ

  visitTypedVariant (TypedVariant meta kind) = AST.Variant meta <$> visitTypedVariantKind kind

  visitTypedVariantKind (TVSimple i) = pure (AST.VSimple i)
  visitTypedVariantKind (TVTuple i ts mt) = AST.VTuple i <$> traverse visitInferredType ts <*> traverse visitInferredType mt
  visitTypedVariantKind (TVStruct i fields mt) = AST.VStruct i <$> traverse visitTypedField fields <*> traverse visitInferredType mt

  visitTypedExpr (TypedExpr meta _ kind) = AST.Expr meta <$> visitTypedExprKind kind

  visitTypedExprKind (TELit l) = pure (AST.ELit l)
  visitTypedExprKind (TEVar i) = pure (AST.EVar i)
  visitTypedExprKind (TEBinOp op e1 e2) = AST.EBinOp op <$> visitTypedExpr e1 <*> visitTypedExpr e2
  visitTypedExprKind (TEUnOp op e) = AST.EUnOp op <$> visitTypedExpr e
  visitTypedExprKind (TEIf c t f) = AST.EIf <$> visitTypedExpr c <*> visitTypedExpr t <*> visitTypedExpr f
  visitTypedExprKind (TEMatch e arms) = AST.EMatch <$> visitTypedExpr e <*> traverse visitTypedMatchArm arms
  visitTypedExprKind (TEBlock es) = AST.EBlock <$> traverse visitTypedExpr es
  visitTypedExprKind (TECall e es) = AST.ECall <$> visitTypedExpr e <*> traverse visitTypedExpr es
  visitTypedExprKind (TELambda ps e) = AST.ELambda <$> traverse visitTypedParam ps <*> visitTypedExpr e
  visitTypedExprKind (TEField e i) = AST.EField <$> visitTypedExpr e <*> pure i
  visitTypedExprKind (TEMethod e i es) = AST.EMethod <$> visitTypedExpr e <*> pure i <*> traverse visitTypedExpr es
  visitTypedExprKind (TESpawn i es) = AST.ESpawn i <$> traverse visitTypedExpr es
  visitTypedExprKind (TESend e1 e2) = AST.ESend <$> visitTypedExpr e1 <*> visitTypedExpr e2
  visitTypedExprKind (TEReceive arms) = AST.EReceive <$> traverse visitTypedMatchArm arms
  visitTypedExprKind (TEMacro i body) = pure (AST.EMacro i body)
  visitTypedExprKind (TELet i t e b) = (AST.ELet i . Just <$> visitInferredType t) <*> visitTypedExpr e <*> pure b
  visitTypedExprKind (TEAssign e1 e2) = AST.EAssign <$> visitTypedExpr e1 <*> visitTypedExpr e2
  visitTypedExprKind (TEFor i e1 e2) = AST.EFor i <$> visitTypedExpr e1 <*> visitTypedExpr e2
  visitTypedExprKind (TEDefer e) = AST.EDefer <$> visitTypedExpr e
  visitTypedExprKind (TEMove e) = AST.EMove <$> visitTypedExpr e
  visitTypedExprKind (TERefMut e) = AST.ERefMut <$> visitTypedExpr e
  visitTypedExprKind (TEAnonRecord fields) = AST.EAnonRecord <$> traverse (\(i, e) -> (,) i <$> visitTypedExpr e) fields
  visitTypedExprKind (TECast e t) = AST.ECast <$> visitTypedExpr e <*> visitInferredType t
  visitTypedExprKind (TEVariant enumName varName args) = AST.EVariant enumName varName <$> traverse visitTypedExpr args
  visitTypedExprKind (TEQuestion e) = AST.EQuestion <$> visitTypedExpr e
  visitTypedExprKind (TEComptime e) = AST.EComptime <$> visitTypedExpr e

  visitTypedPattern (TypedPattern meta _ kind) = AST.Pattern meta <$> visitTypedPatternKind kind

  visitTypedPatternKind (TPVar i) = pure (AST.PVar i)
  visitTypedPatternKind (TPLit l) = pure (AST.PLit l)
  visitTypedPatternKind (TPCon i ps) = AST.PCon i <$> traverse visitTypedPattern ps
  visitTypedPatternKind (TPStruct i fields) = AST.PStruct i <$> traverse (\(j, p) -> (,) j <$> visitTypedPattern p) fields
  visitTypedPatternKind TPWildcard = pure AST.PWildcard

  visitTypedMatchArm (TypedMatchArm meta pat expr) = AST.MatchArm meta <$> visitTypedPattern pat <*> visitTypedExpr expr

  visitTypedDecl (TypedDecl meta kind) = AST.Decl meta <$> visitTypedDeclKind kind

  visitTypedDeclKind (TDFunc i tps ps t e) = AST.DFunc i tps <$> traverse visitTypedParam ps <*> (Just <$> visitInferredType t) <*> visitTypedExpr e
  visitTypedDeclKind (TDStruct i tps fields) = AST.DStruct i tps <$> traverse visitTypedField fields
  visitTypedDeclKind (TDEnum i tps vars) = AST.DEnum i tps <$> traverse visitTypedVariant vars
  visitTypedDeclKind (TDTrait i tps items) = AST.DTrait i tps <$> traverse visitTypedTraitItem items
  visitTypedDeclKind (TDImpl i tps t items) = AST.DImpl i tps <$> visitInferredType t <*> traverse visitTypedImplItem items
  visitTypedDeclKind (TDActor i items) = AST.DActor i <$> traverse visitTypedActorItem items
  visitTypedDeclKind (TDConst i t e) = AST.DConst i <$> visitInferredType t <*> visitTypedExpr e

  visitTypedTraitItem (TypedTraitItem meta kind) = AST.TraitItem meta <$> visitTypedTraitItemKind kind

  visitTypedTraitItemKind (TTFunc i tps ps t me) = AST.TFunc i tps <$> traverse visitTypedParam ps <*> (Just <$> visitInferredType t) <*> traverse visitTypedExpr me
  visitTypedTraitItemKind (TTType i _) = pure (AST.TType i) -- ignore type for untyping trait type

  visitTypedImplItem (TypedImplItem meta kind) = AST.ImplItem meta <$> visitTypedImplItemKind kind

  visitTypedImplItemKind (TIFunc i tps ps t e) = AST.IFunc i tps <$> traverse visitTypedParam ps <*> (Just <$> visitInferredType t) <*> visitTypedExpr e
  visitTypedImplItemKind (TIType i t) = AST.IType i <$> visitInferredType t

  visitTypedActorItem (TypedActorItem meta kind) = AST.ActorItem meta <$> visitTypedActorItemKind kind

  visitTypedActorItemKind (TALet i t e b) = (AST.ALet i . Just <$> visitInferredType t) <*> visitTypedExpr e <*> pure b
  visitTypedActorItemKind (TABehavior i ps e) = AST.ABehavior i <$> traverse visitTypedParam ps <*> visitTypedExpr e
  visitTypedActorItemKind (TAReceive arms) = AST.AReceive <$> traverse visitTypedMatchArm arms

  visitTypedProgram (TypedProgram imps decls) = AST.Program imps <$> traverse visitTypedDecl decls

-- | Get the type of a typed expression
typeOf :: TypedExpr -> InferredType
typeOf (TypedExpr _ t _) = t

typeOfPattern :: TypedPattern -> InferredType
typeOfPattern (TypedPattern _ t _) = t

-- _Untypes your typed expression_
untypeType :: InferredType -> AST.Type
untypeType = runUntype . visitInferredType

untypeTypedParam :: TypedParam -> AST.Param
untypeTypedParam = runUntype . visitTypedParam

untypeTypedField :: TypedField -> AST.Field
untypeTypedField = runUntype . visitTypedField

untypeTypedVariant :: TypedVariant -> AST.Variant
untypeTypedVariant = runUntype . visitTypedVariant

untypeExpr :: TypedExpr -> AST.Expr
untypeExpr = runUntype . visitTypedExpr

-- | Extract the untyped pattern from a typed pattern
untypePattern :: TypedPattern -> AST.Pattern
untypePattern = runUntype . visitTypedPattern

-- | Extract the untyped declaration from a typed declaration
untypeDecl :: TypedDecl -> AST.Decl
untypeDecl = runUntype . visitTypedDecl

-- | Extract the untyped program from a typed program
untypeProgram :: TypedProgram -> AST.Program
untypeProgram = runUntype . visitTypedProgram
