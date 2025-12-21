{-# LANGUAGE ConstraintKinds #-}

module Vial.AST where

type Ident = String

type SourceFile = String

data Span where
  Span :: {spanStart :: Int, spanEnd :: Int} -> Span
  deriving (Show, Eq)

data Attribute = Attribute
  { attrName :: Ident,
    attrArgs :: [Expr]
  }
  deriving (Show, Eq)

data Metadata where
  Metadata ::
    { metaSpan :: Span,
      metaFile :: SourceFile,
      metaAttributes :: [Attribute]
    } ->
    Metadata
  deriving (Show, Eq)

data Kind
  = KStar
  | KArr Kind Kind
  deriving (Show, Eq)

data Type where
  Type :: {typeMeta :: Metadata, typeKind :: TypeKind} -> Type
  deriving (Show, Eq)

data TypeKind where
  TyVar :: Ident -> TypeKind
  TyCon :: Ident -> [Type] -> TypeKind
  TyApp :: Type -> Type -> TypeKind
  TyFunc :: [Type] -> Type -> TypeKind
  TyOption :: Type -> TypeKind
  TyRecord :: [(Ident, Type)] -> (Maybe Ident) -> TypeKind
  TyKinded :: Ident -> Kind -> TypeKind
  TyDyn :: Ident -> TypeKind
  deriving (Show, Eq)

data Literal where
  LInt :: Int -> Literal
  LFloat :: Double -> Literal
  LString :: String -> Literal
  LBool :: Bool -> Literal
  deriving (Show, Eq)

data BinOp where
  Add :: BinOp
  Sub :: BinOp
  Mul :: BinOp
  Div :: BinOp
  Mod :: BinOp
  Eq :: BinOp
  Neq :: BinOp
  Lt :: BinOp
  Gt :: BinOp
  Le :: BinOp
  Ge :: BinOp
  And :: BinOp
  Or :: BinOp
  BitAnd :: BinOp
  BitOr :: BinOp
  BitXor :: BinOp
  Shl :: BinOp
  Shr :: BinOp
  Pipe :: BinOp
  deriving (Show, Eq)

data UnOp = Neg | Not
  deriving (Show, Eq)

data Pattern where
  Pattern :: {patMeta :: Metadata, patKind :: PatternKind} -> Pattern
  deriving (Show, Eq)

data PatternKind
  = PVar Ident
  | PLit Literal
  | PCon Ident [Pattern]
  | PStruct Ident [(Ident, Pattern)]
  | PWildcard
  deriving (Show, Eq)

data Expr where
  Expr :: {exprMeta :: Metadata, exprKind :: ExprKind} -> Expr
  deriving (Show, Eq)

data ExprKind where
  ELit :: Literal -> ExprKind
  EVar :: Ident -> ExprKind
  EBinOp :: BinOp -> Expr -> Expr -> ExprKind
  EUnOp :: UnOp -> Expr -> ExprKind
  EIf :: Expr -> Expr -> Expr -> ExprKind
  EMatch :: Expr -> [MatchArm] -> ExprKind
  EBlock :: [Expr] -> ExprKind
  ECall :: Expr -> [Expr] -> ExprKind
  ELambda :: [Param] -> Expr -> ExprKind
  EField :: Expr -> Ident -> ExprKind
  EMethod :: Expr -> Ident -> [Expr] -> ExprKind
  ESpawn :: Ident -> [Expr] -> ExprKind
  ESend :: Expr -> Expr -> ExprKind
  EReceive :: [MatchArm] -> ExprKind
  EMacro :: Ident -> MacroBody -> ExprKind
  ELet :: Ident -> (Maybe Type) -> Expr -> Bool -> ExprKind
  EAssign :: Expr -> Expr -> ExprKind
  EFor :: Ident -> Expr -> Expr -> ExprKind
  EDefer :: Expr -> ExprKind
  EMove :: Expr -> ExprKind
  ERefMut :: Expr -> ExprKind
  EAnonRecord :: [(Ident, Expr)] -> ExprKind
  ECast :: Expr -> Type -> ExprKind
  EVariant :: Ident -> Ident -> [Expr] -> ExprKind
  EQuestion :: Expr -> ExprKind
  EComptime :: Expr -> ExprKind
  deriving (Show, Eq)

data MacroBody where
  MExprs :: [Expr] -> MacroBody
  MString :: String -> MacroBody
  MBlock :: [Expr] -> MacroBody
  deriving (Show, Eq)

data MatchArm where
  MatchArm ::
    { armMeta :: Metadata,
      armPat :: Pattern,
      armExpr :: Expr
    } ->
    MatchArm
  deriving (Show, Eq)

data Decl where
  Decl :: {declMeta :: Metadata, declKind :: DeclKind} -> Decl
  deriving (Show, Eq)

data DeclKind where
  DFunc :: Ident -> [TypeParam] -> [Param] -> (Maybe Type) -> Expr -> DeclKind
  DStruct :: Ident -> [TypeParam] -> [Field] -> DeclKind
  DEnum :: Ident -> [TypeParam] -> [Variant] -> DeclKind
  DTrait :: Ident -> [TypeParam] -> [TraitItem] -> DeclKind
  DImpl :: Ident -> [TypeParam] -> Type -> [ImplItem] -> DeclKind
  DActor :: Ident -> [ActorItem] -> DeclKind
  DConst :: Ident -> Type -> Expr -> DeclKind
  deriving (Show, Eq)

data TypeParam where
  TypeParam :: {tpMeta :: Metadata, tpName :: Ident, tpKind :: Maybe Kind} -> TypeParam
  deriving (Show, Eq)

data Param where
  Param :: {paramMeta :: Metadata, paramName :: Ident, paramType :: Type, paramMut :: Bool} -> Param
  deriving (Show, Eq)

data Field where
  Field :: {fieldMeta :: Metadata, fieldName :: Ident, fieldType :: Type} -> Field
  deriving (Show, Eq)

data Variant where
  Variant :: {varMeta :: Metadata, varKind :: VariantKind} -> Variant
  deriving (Show, Eq)

data VariantKind where
  VSimple :: Ident -> VariantKind
  VTuple :: Ident -> [Type] -> (Maybe Type) -> VariantKind
  VStruct :: Ident -> [Field] -> (Maybe Type) -> VariantKind
  deriving (Show, Eq)

data TraitItem where
  TraitItem :: {tiMeta :: Metadata, tiKind :: TraitItemKind} -> TraitItem
  deriving (Show, Eq)

data TraitItemKind where
  TFunc :: Ident -> [TypeParam] -> [Param] -> (Maybe Type) -> (Maybe Expr) -> TraitItemKind
  TType :: Ident -> TraitItemKind
  deriving (Show, Eq)

data ImplItem where
  ImplItem :: {iiMeta :: Metadata, iiKind :: ImplItemKind} -> ImplItem
  deriving (Show, Eq)

data ImplItemKind where
  IFunc :: Ident -> [TypeParam] -> [Param] -> (Maybe Type) -> Expr -> ImplItemKind
  IType :: Ident -> Type -> ImplItemKind
  deriving (Show, Eq)

data ActorItem where
  ActorItem :: {aiMeta :: Metadata, aiKind :: ActorItemKind} -> ActorItem
  deriving (Show, Eq)

data ActorItemKind where
  ALet :: Ident -> (Maybe Type) -> Expr -> Bool -> ActorItemKind
  ABehavior :: Ident -> [Param] -> Expr -> ActorItemKind
  AReceive :: [MatchArm] -> ActorItemKind
  deriving (Show, Eq)

data Program where
  Program :: {pImports :: [Import], pDecls :: [Decl]} -> Program
  deriving (Show, Eq)

data Import where
  Import :: {impMeta :: Metadata, impKind :: ImportKind} -> Import
  deriving (Show, Eq)

data ImportKind where
  ImportSimple :: [Ident] -> (Maybe Ident) -> ImportKind
  ImportQualified :: [Ident] -> [Ident] -> ImportKind
  deriving (Show, Eq)

class (Monad m) => Visitor m where
  visitKind :: Kind -> m Kind
  visitKind = defaultVisitKind

  visitType :: Type -> m Type
  visitType = defaultVisitType

  visitTypeKind :: TypeKind -> m TypeKind
  visitTypeKind = defaultVisitTypeKind

  visitLiteral :: Literal -> m Literal
  visitLiteral = defaultVisitLiteral

  visitBinOp :: BinOp -> m BinOp
  visitBinOp = defaultVisitBinOp

  visitUnOp :: UnOp -> m UnOp
  visitUnOp = defaultVisitUnOp

  visitPattern :: Pattern -> m Pattern
  visitPattern = defaultVisitPattern

  visitPatternKind :: PatternKind -> m PatternKind
  visitPatternKind = defaultVisitPatternKind

  visitExpr :: Expr -> m Expr
  visitExpr = defaultVisitExpr

  visitExprKind :: ExprKind -> m ExprKind
  visitExprKind = defaultVisitExprKind

  visitMacroBody :: MacroBody -> m MacroBody
  visitMacroBody = defaultVisitMacroBody

  visitMatchArm :: MatchArm -> m MatchArm
  visitMatchArm = defaultVisitMatchArm

  visitDecl :: Decl -> m Decl
  visitDecl = defaultVisitDecl

  visitDeclKind :: DeclKind -> m DeclKind
  visitDeclKind = defaultVisitDeclKind

  visitTypeParam :: TypeParam -> m TypeParam
  visitTypeParam = defaultVisitTypeParam

  visitParam :: Param -> m Param
  visitParam = defaultVisitParam

  visitField :: Field -> m Field
  visitField = defaultVisitField

  visitVariant :: Variant -> m Variant
  visitVariant = defaultVisitVariant

  visitVariantKind :: VariantKind -> m VariantKind
  visitVariantKind = defaultVisitVariantKind

  visitTraitItem :: TraitItem -> m TraitItem
  visitTraitItem = defaultVisitTraitItem

  visitTraitItemKind :: TraitItemKind -> m TraitItemKind
  visitTraitItemKind = defaultVisitTraitItemKind

  visitImplItem :: ImplItem -> m ImplItem
  visitImplItem = defaultVisitImplItem

  visitImplItemKind :: ImplItemKind -> m ImplItemKind
  visitImplItemKind = defaultVisitImplItemKind

  visitActorItem :: ActorItem -> m ActorItem
  visitActorItem = defaultVisitActorItem

  visitActorItemKind :: ActorItemKind -> m ActorItemKind
  visitActorItemKind = defaultVisitActorItemKind

  visitProgram :: Program -> m Program
  visitProgram = defaultVisitProgram

  visitImport :: Import -> m Import
  visitImport = defaultVisitImport

  visitImportKind :: ImportKind -> m ImportKind
  visitImportKind = defaultVisitImportKind

defaultVisitKind :: (Visitor m) => Kind -> m Kind
defaultVisitKind KStar = pure KStar
defaultVisitKind (KArr k1 k2) = KArr <$> visitKind k1 <*> visitKind k2

defaultVisitType :: (Visitor m) => Type -> m Type
defaultVisitType (Type meta kind) = Type meta <$> visitTypeKind kind

defaultVisitTypeKind :: (Visitor m) => TypeKind -> m TypeKind
defaultVisitTypeKind (TyVar i) = pure (TyVar i)
defaultVisitTypeKind (TyCon i ts) = TyCon i <$> traverse visitType ts
defaultVisitTypeKind (TyApp t1 t2) = TyApp <$> visitType t1 <*> visitType t2
defaultVisitTypeKind (TyFunc ts t) = TyFunc <$> traverse visitType ts <*> visitType t
defaultVisitTypeKind (TyOption t) = TyOption <$> visitType t
defaultVisitTypeKind (TyRecord fields m) = TyRecord <$> traverse (\(i, t) -> (,) i <$> visitType t) fields <*> pure m
defaultVisitTypeKind (TyKinded i k) = TyKinded i <$> visitKind k
defaultVisitTypeKind (TyDyn i) = pure (TyDyn i)

defaultVisitLiteral :: (Visitor m) => Literal -> m Literal
defaultVisitLiteral = pure

defaultVisitBinOp :: (Visitor m) => BinOp -> m BinOp
defaultVisitBinOp = pure

defaultVisitUnOp :: (Visitor m) => UnOp -> m UnOp
defaultVisitUnOp = pure

defaultVisitPattern :: (Visitor m) => Pattern -> m Pattern
defaultVisitPattern (Pattern meta kind) = Pattern meta <$> visitPatternKind kind

defaultVisitPatternKind :: (Visitor m) => PatternKind -> m PatternKind
defaultVisitPatternKind (PVar i) = pure (PVar i)
defaultVisitPatternKind (PLit l) = PLit <$> visitLiteral l
defaultVisitPatternKind (PCon i ps) = PCon i <$> traverse visitPattern ps
defaultVisitPatternKind (PStruct i fields) = PStruct i <$> traverse (\(j, p) -> (,) j <$> visitPattern p) fields
defaultVisitPatternKind PWildcard = pure PWildcard

defaultVisitExpr :: (Visitor m) => Expr -> m Expr
defaultVisitExpr (Expr meta kind) = Expr meta <$> visitExprKind kind

defaultVisitExprKind :: (Visitor m) => ExprKind -> m ExprKind
defaultVisitExprKind (ELit l) = ELit <$> visitLiteral l
defaultVisitExprKind (EVar i) = pure (EVar i)
defaultVisitExprKind (EBinOp op e1 e2) = EBinOp <$> visitBinOp op <*> visitExpr e1 <*> visitExpr e2
defaultVisitExprKind (EUnOp op e) = EUnOp <$> visitUnOp op <*> visitExpr e
defaultVisitExprKind (EIf c t f) = EIf <$> visitExpr c <*> visitExpr t <*> visitExpr f
defaultVisitExprKind (EMatch e arms) = EMatch <$> visitExpr e <*> traverse visitMatchArm arms
defaultVisitExprKind (EBlock es) = EBlock <$> traverse visitExpr es
defaultVisitExprKind (ECall e es) = ECall <$> visitExpr e <*> traverse visitExpr es
defaultVisitExprKind (ELambda ps e) = ELambda <$> traverse visitParam ps <*> visitExpr e
defaultVisitExprKind (EField e i) = EField <$> visitExpr e <*> pure i
defaultVisitExprKind (EMethod e i es) = EMethod <$> visitExpr e <*> pure i <*> traverse visitExpr es
defaultVisitExprKind (ESpawn i es) = ESpawn i <$> traverse visitExpr es
defaultVisitExprKind (ESend e1 e2) = ESend <$> visitExpr e1 <*> visitExpr e2
defaultVisitExprKind (EReceive arms) = EReceive <$> traverse visitMatchArm arms
defaultVisitExprKind (EMacro i body) = EMacro i <$> visitMacroBody body
defaultVisitExprKind (ELet i mt e b) = ELet i <$> traverse visitType mt <*> visitExpr e <*> pure b
defaultVisitExprKind (EAssign e1 e2) = EAssign <$> visitExpr e1 <*> visitExpr e2
defaultVisitExprKind (EFor i e1 e2) = EFor i <$> visitExpr e1 <*> visitExpr e2
defaultVisitExprKind (EDefer e) = EDefer <$> visitExpr e
defaultVisitExprKind (EMove e) = EMove <$> visitExpr e
defaultVisitExprKind (ERefMut e) = ERefMut <$> visitExpr e
defaultVisitExprKind (EAnonRecord fields) = EAnonRecord <$> traverse (\(i, e) -> (,) i <$> visitExpr e) fields
defaultVisitExprKind (ECast e t) = ECast <$> visitExpr e <*> visitType t
defaultVisitExprKind (EVariant enumName varName args) = EVariant enumName varName <$> traverse visitExpr args
defaultVisitExprKind (EQuestion e) = EQuestion <$> visitExpr e
defaultVisitExprKind (EComptime e) = EComptime <$> visitExpr e

defaultVisitMacroBody :: (Visitor m) => MacroBody -> m MacroBody
defaultVisitMacroBody (MExprs es) = MExprs <$> traverse visitExpr es
defaultVisitMacroBody (MString s) = pure (MString s)
defaultVisitMacroBody (MBlock es) = MBlock <$> traverse visitExpr es

defaultVisitMatchArm :: (Visitor m) => MatchArm -> m MatchArm
defaultVisitMatchArm (MatchArm meta pat expr) = MatchArm meta <$> visitPattern pat <*> visitExpr expr

defaultVisitDecl :: (Visitor m) => Decl -> m Decl
defaultVisitDecl (Decl meta kind) = Decl meta <$> visitDeclKind kind

defaultVisitDeclKind :: (Visitor m) => DeclKind -> m DeclKind
defaultVisitDeclKind (DFunc i tps ps mt e) = DFunc i <$> traverse visitTypeParam tps <*> traverse visitParam ps <*> traverse visitType mt <*> visitExpr e
defaultVisitDeclKind (DStruct i tps fields) = DStruct i <$> traverse visitTypeParam tps <*> traverse visitField fields
defaultVisitDeclKind (DEnum i tps vars) = DEnum i <$> traverse visitTypeParam tps <*> traverse visitVariant vars
defaultVisitDeclKind (DTrait i tps items) = DTrait i <$> traverse visitTypeParam tps <*> traverse visitTraitItem items
defaultVisitDeclKind (DImpl i tps t items) = DImpl i <$> traverse visitTypeParam tps <*> visitType t <*> traverse visitImplItem items
defaultVisitDeclKind (DActor i items) = DActor i <$> traverse visitActorItem items
defaultVisitDeclKind (DConst i t e) = DConst i <$> visitType t <*> visitExpr e

defaultVisitTypeParam :: (Visitor m) => TypeParam -> m TypeParam
defaultVisitTypeParam (TypeParam meta name mk) = TypeParam meta name <$> traverse visitKind mk

defaultVisitParam :: (Visitor m) => Param -> m Param
defaultVisitParam (Param meta name typ mut) = Param meta name <$> visitType typ <*> pure mut

defaultVisitField :: (Visitor m) => Field -> m Field
defaultVisitField (Field meta name typ) = Field meta name <$> visitType typ

defaultVisitVariant :: (Visitor m) => Variant -> m Variant
defaultVisitVariant (Variant meta kind) = Variant meta <$> visitVariantKind kind

defaultVisitVariantKind :: (Visitor m) => VariantKind -> m VariantKind
defaultVisitVariantKind (VSimple i) = pure (VSimple i)
defaultVisitVariantKind (VTuple i ts mt) = VTuple i <$> traverse visitType ts <*> traverse visitType mt
defaultVisitVariantKind (VStruct i fields mt) = VStruct i <$> traverse visitField fields <*> traverse visitType mt

defaultVisitTraitItem :: (Visitor m) => TraitItem -> m TraitItem
defaultVisitTraitItem (TraitItem meta kind) = TraitItem meta <$> visitTraitItemKind kind

defaultVisitTraitItemKind :: (Visitor m) => TraitItemKind -> m TraitItemKind
defaultVisitTraitItemKind (TFunc i tps ps mt me) = TFunc i <$> traverse visitTypeParam tps <*> traverse visitParam ps <*> traverse visitType mt <*> traverse visitExpr me
defaultVisitTraitItemKind (TType i) = pure (TType i)

defaultVisitImplItem :: (Visitor m) => ImplItem -> m ImplItem
defaultVisitImplItem (ImplItem meta kind) = ImplItem meta <$> visitImplItemKind kind

defaultVisitImplItemKind :: (Visitor m) => ImplItemKind -> m ImplItemKind
defaultVisitImplItemKind (IFunc i tps ps mt e) = IFunc i <$> traverse visitTypeParam tps <*> traverse visitParam ps <*> traverse visitType mt <*> visitExpr e
defaultVisitImplItemKind (IType i t) = IType i <$> visitType t

defaultVisitActorItem :: (Visitor m) => ActorItem -> m ActorItem
defaultVisitActorItem (ActorItem meta kind) = ActorItem meta <$> visitActorItemKind kind

defaultVisitActorItemKind :: (Visitor m) => ActorItemKind -> m ActorItemKind
defaultVisitActorItemKind (ALet i mt e b) = ALet i <$> traverse visitType mt <*> visitExpr e <*> pure b
defaultVisitActorItemKind (ABehavior i ps e) = ABehavior i <$> traverse visitParam ps <*> visitExpr e
defaultVisitActorItemKind (AReceive arms) = AReceive <$> traverse visitMatchArm arms

defaultVisitProgram :: (Visitor m) => Program -> m Program
defaultVisitProgram (Program imps decls) = Program <$> traverse visitImport imps <*> traverse visitDecl decls

defaultVisitImport :: (Visitor m) => Import -> m Import
defaultVisitImport (Import meta kind) = Import meta <$> visitImportKind kind

defaultVisitImportKind :: (Visitor m) => ImportKind -> m ImportKind
defaultVisitImportKind (ImportSimple ids mi) = pure (ImportSimple ids mi)
defaultVisitImportKind (ImportQualified ids1 ids2) = pure (ImportQualified ids1 ids2)

-- Identity visitor for tree walking
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

-- Simple instance uses all defaults
instance Visitor Identity
