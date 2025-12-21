module Vial.Desugar (desugar) where

import Vial.AST

newtype Desugar a = Desugar { runDesugar :: a }

instance Functor Desugar where
  fmap f (Desugar a) = Desugar (f a)

instance Applicative Desugar where
  pure = Desugar
  Desugar f <*> Desugar a = Desugar (f a)

instance Monad Desugar where
  return = pure
  Desugar a >>= f = f a

desugar :: Program -> Program
desugar = runDesugar . visitProgram

-- This instance handles ALL types via defaults (defined in the class),
-- but we override just 'visitExprKind' to handle pipes.
instance Visitor Desugar where
  visitExprKind (EBinOp Pipe e1 e2) =
    let e1' = runDesugar (visitExpr e1)
        e2' = runDesugar (visitExpr e2)
     in Desugar $ case exprKind e2' of
          ECall func args -> ECall func (e1' : args)
          _ -> ECall e2' [e1']
  visitExprKind other = defaultVisitExprKind other
