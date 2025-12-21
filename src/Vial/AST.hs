{-# OPTIONS_GHC -Wno-missing-methods #-}

module Vial.AST where

type Ident = String

type SourceFile = String

data Span where
  Span :: {spanStart :: Int, spanEnd :: Int} -> Span
  deriving (Show, Eq)

-- Attributes (e.g., @serde(name: "foo")).
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

-- Kinds don't need metadata
data Kind
  = -- | *
    KStar
  | -- | k -> k
    KArr Kind Kind
  deriving (Show, Eq)

-- Types
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
  = --  x
    PVar Ident
  | -- | 42, "hello"
    PLit Literal
  | -- | Status::Pending, Option::Some(x)
    PCon Ident [Pattern]
  | -- | User { id: 1, .. }
    PStruct Ident [(Ident, Pattern)]
  | -- | _
    PWildcard
  deriving (Show, Eq)

-- Expressions
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
  DFunc ::
    Ident ->
    [TypeParam] ->
    [Param] ->
    (Maybe Type) ->
    Expr ->
    DeclKind
  DStruct :: Ident -> [TypeParam] -> [Field] -> DeclKind
  DEnum :: Ident -> [TypeParam] -> [Variant] -> DeclKind
  DTrait :: Ident -> [TypeParam] -> [TraitItem] -> DeclKind
  DImpl :: Ident -> [TypeParam] -> Type -> [ImplItem] -> DeclKind
  DActor :: Ident -> [ActorItem] -> DeclKind
  deriving (Show, Eq)

data TypeParam where
  TypeParam ::
    { tpMeta :: Metadata,
      tpName :: Ident,
      tpKind :: Maybe Kind
    } ->
    TypeParam
  deriving (Show, Eq)

data Param where
  Param ::
    { paramMeta :: Metadata,
      paramName :: Ident,
      paramType :: Type,
      paramMut :: Bool
    } ->
    Param
  deriving (Show, Eq)

data Field where
  Field ::
    { fieldMeta :: Metadata,
      fieldName :: Ident,
      fieldType :: Type
    } ->
    Field
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
  TraitItem ::
    {tiMeta :: Metadata, tiKind :: TraitItemKind} ->
    TraitItem
  deriving (Show, Eq)

data TraitItemKind where
  TFunc ::
    Ident ->
    [TypeParam] ->
    [Param] ->
    (Maybe Type) ->
    (Maybe Expr) ->
    TraitItemKind
  TType :: Ident -> TraitItemKind
  deriving (Show, Eq)

data ImplItem where
  ImplItem ::
    {iiMeta :: Metadata, iiKind :: ImplItemKind} ->
    ImplItem
  deriving (Show, Eq)

data ImplItemKind where
  IFunc ::
    Ident ->
    [TypeParam] ->
    [Param] ->
    (Maybe Type) ->
    Expr ->
    ImplItemKind
  IType :: Ident -> Type -> ImplItemKind
  deriving (Show, Eq)

data ActorItem where
  ActorItem ::
    {aiMeta :: Metadata, aiKind :: ActorItemKind} ->
    ActorItem
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

class Visitor r where
  visitKind :: Kind -> r
  visitType :: Type -> r
  visitTypeKind :: TypeKind -> r
  visitLiteral :: Literal -> r
  visitBinOp :: BinOp -> r
  visitUnOp :: UnOp -> r
  visitPattern :: Pattern -> r
  visitPatternKind :: PatternKind -> r
  visitExpr :: Expr -> r
  visitExprKind :: ExprKind -> r
  visitMacroBody :: MacroBody -> r
  visitMatchArm :: MatchArm -> r
  visitDecl :: Decl -> r
  visitDeclKind :: DeclKind -> r
  visitTypeParam :: TypeParam -> r
  visitParam :: Param -> r
  visitField :: Field -> r
  visitVariant :: Variant -> r
  visitVariantKind :: VariantKind -> r
  visitTraitItem :: TraitItem -> r
  visitTraitItemKind :: TraitItemKind -> r
  visitImplItem :: ImplItem -> r
  visitImplItemKind :: ImplItemKind -> r
  visitActorItem :: ActorItem -> r
  visitActorItemKind :: ActorItemKind -> r
  visitProgram :: Program -> r
  visitImport :: Import -> r
  visitImportKind :: ImportKind -> r

-- Identity visitor for tree walking
newtype Identity a = Identity {runIdentity :: a}

instance Visitor (Identity Kind) where
  visitKind KStar = Identity KStar
  visitKind (KArr k1 k2) = Identity (KArr (runIdentity (visitKind k1)) (runIdentity (visitKind k2)))

instance Visitor (Identity Type) where
  visitType (Type meta kind) = Identity (Type meta (runIdentity (visitTypeKind kind)))

instance Visitor (Identity TypeKind) where
  visitTypeKind (TyVar i) = Identity (TyVar i)
  visitTypeKind (TyCon i ts) = Identity (TyCon i (map (runIdentity . visitType) ts))
  visitTypeKind (TyApp t1 t2) = Identity (TyApp (runIdentity (visitType t1)) (runIdentity (visitType t2)))
  visitTypeKind (TyFunc ts t) = Identity (TyFunc (map (runIdentity . visitType) ts) (runIdentity (visitType t)))
  visitTypeKind (TyOption t) = Identity (TyOption (runIdentity (visitType t)))
  visitTypeKind (TyRecord fields m) = Identity (TyRecord (map (\(i, t) -> (i, runIdentity (visitType t))) fields) m)
  visitTypeKind (TyKinded i k) = Identity (TyKinded i (runIdentity (visitKind k)))

instance Visitor (Identity Literal) where
  visitLiteral = Identity

instance Visitor (Identity BinOp) where
  visitBinOp = Identity

instance Visitor (Identity UnOp) where
  visitUnOp = Identity

instance Visitor (Identity Pattern) where
  visitPattern (Pattern meta kind) = Identity (Pattern meta (runIdentity (visitPatternKind kind)))

instance Visitor (Identity PatternKind) where
  visitPatternKind (PVar i) = Identity (PVar i)
  visitPatternKind (PLit l) = Identity (PLit (runIdentity (visitLiteral l)))
  visitPatternKind (PCon i ps) = Identity (PCon i (map (runIdentity . visitPattern) ps))
  visitPatternKind (PStruct i fields) = Identity (PStruct i (map (\(j, p) -> (j, runIdentity (visitPattern p))) fields))
  visitPatternKind PWildcard = Identity PWildcard

instance Visitor (Identity Expr) where
  visitExpr (Expr meta kind) = Identity (Expr meta (runIdentity (visitExprKind kind)))

instance Visitor (Identity ExprKind) where
  visitExprKind (ELit l) = Identity (ELit (runIdentity (visitLiteral l)))
  visitExprKind (EVar i) = Identity (EVar i)
  visitExprKind (EBinOp op e1 e2) = Identity (EBinOp (runIdentity (visitBinOp op)) (runIdentity (visitExpr e1)) (runIdentity (visitExpr e2)))
  visitExprKind (EUnOp op e) = Identity (EUnOp (runIdentity (visitUnOp op)) (runIdentity (visitExpr e)))
  visitExprKind (EIf c t f) = Identity (EIf (runIdentity (visitExpr c)) (runIdentity (visitExpr t)) (runIdentity (visitExpr f)))
  visitExprKind (EMatch e arms) = Identity (EMatch (runIdentity (visitExpr e)) (map (runIdentity . visitMatchArm) arms))
  visitExprKind (EBlock es) = Identity (EBlock (map (runIdentity . visitExpr) es))
  visitExprKind (ECall e es) = Identity (ECall (runIdentity (visitExpr e)) (map (runIdentity . visitExpr) es))
  visitExprKind (ELambda ps e) = Identity (ELambda (map (runIdentity . visitParam) ps) (runIdentity (visitExpr e)))
  visitExprKind (EField e i) = Identity (EField (runIdentity (visitExpr e)) i)
  visitExprKind (EMethod e i es) = Identity (EMethod (runIdentity (visitExpr e)) i (map (runIdentity . visitExpr) es))
  visitExprKind (ESpawn i es) = Identity (ESpawn i (map (runIdentity . visitExpr) es))
  visitExprKind (ESend e1 e2) = Identity (ESend (runIdentity (visitExpr e1)) (runIdentity (visitExpr e2)))
  visitExprKind (EReceive arms) = Identity (EReceive (map (runIdentity . visitMatchArm) arms))
  visitExprKind (EMacro i body) = Identity (EMacro i (runIdentity (visitMacroBody body)))
  visitExprKind (ELet i mt e b) = Identity (ELet i (fmap (runIdentity . visitType) mt) (runIdentity (visitExpr e)) b)
  visitExprKind (EAssign e1 e2) = Identity (EAssign (runIdentity (visitExpr e1)) (runIdentity (visitExpr e2)))
  visitExprKind (EFor i e1 e2) = Identity (EFor i (runIdentity (visitExpr e1)) (runIdentity (visitExpr e2)))
  visitExprKind (EDefer e) = Identity (EDefer (runIdentity (visitExpr e)))
  visitExprKind (EMove e) = Identity (EMove (runIdentity (visitExpr e)))
  visitExprKind (ERefMut e) = Identity (ERefMut (runIdentity (visitExpr e)))
  visitExprKind (EAnonRecord fields) = Identity (EAnonRecord (map (\(i, e) -> (i, runIdentity (visitExpr e))) fields))
  visitExprKind (ECast e t) = Identity (ECast (runIdentity (visitExpr e)) (runIdentity (visitType t)))

instance Visitor (Identity MacroBody) where
  visitMacroBody (MExprs es) = Identity (MExprs (map (runIdentity . visitExpr) es))
  visitMacroBody (MString s) = Identity (MString s)
  visitMacroBody (MBlock es) = Identity (MBlock (map (runIdentity . visitExpr) es))

instance Visitor (Identity MatchArm) where
  visitMatchArm (MatchArm meta pat expr) = Identity (MatchArm meta (runIdentity (visitPattern pat)) (runIdentity (visitExpr expr)))

instance Visitor (Identity Decl) where
  visitDecl (Decl meta kind) = Identity (Decl meta (runIdentity (visitDeclKind kind)))

instance Visitor (Identity DeclKind) where
  visitDeclKind (DFunc i tps ps mt e) = Identity (DFunc i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitParam) ps) (fmap (runIdentity . visitType) mt) (runIdentity (visitExpr e)))
  visitDeclKind (DStruct i tps fields) = Identity (DStruct i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitField) fields))
  visitDeclKind (DEnum i tps vars) = Identity (DEnum i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitVariant) vars))
  visitDeclKind (DTrait i tps items) = Identity (DTrait i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitTraitItem) items))
  visitDeclKind (DImpl i tps t items) = Identity (DImpl i (map (runIdentity . visitTypeParam) tps) (runIdentity (visitType t)) (map (runIdentity . visitImplItem) items))
  visitDeclKind (DActor i items) = Identity (DActor i (map (runIdentity . visitActorItem) items))

instance Visitor (Identity TypeParam) where
  visitTypeParam (TypeParam meta name mk) = Identity (TypeParam meta name (fmap (runIdentity . visitKind) mk))

instance Visitor (Identity Param) where
  visitParam (Param meta name typ mut) = Identity (Param meta name (runIdentity (visitType typ)) mut)

instance Visitor (Identity Field) where
  visitField (Field meta name typ) = Identity (Field meta name (runIdentity (visitType typ)))

instance Visitor (Identity Variant) where
  visitVariant (Variant meta kind) = Identity (Variant meta (runIdentity (visitVariantKind kind)))

instance Visitor (Identity VariantKind) where
  visitVariantKind (VSimple i) = Identity (VSimple i)
  visitVariantKind (VTuple i ts mt) = Identity (VTuple i (map (runIdentity . visitType) ts) (fmap (runIdentity . visitType) mt))
  visitVariantKind (VStruct i fields mt) = Identity (VStruct i (map (runIdentity . visitField) fields) (fmap (runIdentity . visitType) mt))

instance Visitor (Identity TraitItem) where
  visitTraitItem (TraitItem meta kind) = Identity (TraitItem meta (runIdentity (visitTraitItemKind kind)))

instance Visitor (Identity TraitItemKind) where
  visitTraitItemKind (TFunc i tps ps mt me) = Identity (TFunc i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitParam) ps) (fmap (runIdentity . visitType) mt) (fmap (runIdentity . visitExpr) me))
  visitTraitItemKind (TType i) = Identity (TType i)

instance Visitor (Identity ImplItem) where
  visitImplItem (ImplItem meta kind) = Identity (ImplItem meta (runIdentity (visitImplItemKind kind)))

instance Visitor (Identity ImplItemKind) where
  visitImplItemKind (IFunc i tps ps mt e) = Identity (IFunc i (map (runIdentity . visitTypeParam) tps) (map (runIdentity . visitParam) ps) (fmap (runIdentity . visitType) mt) (runIdentity (visitExpr e)))
  visitImplItemKind (IType i t) = Identity (IType i (runIdentity (visitType t)))

instance Visitor (Identity ActorItem) where
  visitActorItem (ActorItem meta kind) = Identity (ActorItem meta (runIdentity (visitActorItemKind kind)))

instance Visitor (Identity ActorItemKind) where
  visitActorItemKind (ALet i mt e b) = Identity (ALet i (fmap (runIdentity . visitType) mt) (runIdentity (visitExpr e)) b)
  visitActorItemKind (ABehavior i ps e) = Identity (ABehavior i (map (runIdentity . visitParam) ps) (runIdentity (visitExpr e)))
  visitActorItemKind (AReceive arms) = Identity (AReceive (map (runIdentity . visitMatchArm) arms))

instance Visitor (Identity Program) where
  visitProgram (Program imps decls) = Identity (Program (map (runIdentity . visitImport) imps) (map (runIdentity . visitDecl) decls))

instance Visitor (Identity Import) where
  visitImport (Import meta kind) = Identity (Import meta (runIdentity (visitImportKind kind)))

instance Visitor (Identity ImportKind) where
  visitImportKind (ImportSimple ids mi) = Identity (ImportSimple ids mi)
  visitImportKind (ImportQualified ids1 ids2) = Identity (ImportQualified ids1 ids2)
