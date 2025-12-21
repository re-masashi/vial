module Main (main) where

import Vial.AST
import Vial.Desugar
import Control.Monad (unless)

dummyMeta :: Metadata
dummyMeta = Metadata (Span 0 0) "test" []

mkExpr :: ExprKind -> Expr
mkExpr k = Expr dummyMeta k

mkVar :: String -> Expr
mkVar s = mkExpr (EVar s)

mkCall :: Expr -> [Expr] -> Expr
mkCall f args = mkExpr (ECall f args)

mkPipe :: Expr -> Expr -> Expr
mkPipe e1 e2 = mkExpr (EBinOp Pipe e1 e2)

mkProgram :: Expr -> Program
mkProgram e = Program [] [Decl dummyMeta (DFunc "main" [] [] Nothing e)]

getExprFromProgram :: Program -> Expr
getExprFromProgram (Program _ [Decl _ (DFunc _ _ _ _ e)]) = e
getExprFromProgram _ = error "Unexpected program structure"

testPipeSimple :: IO ()
testPipeSimple = do
  -- a |> b  ->  b(a)
  let a = mkVar "a"
      b = mkVar "b"
      input = mkProgram (mkPipe a b)
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = ECall b [a]
      
  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testPipeSimple failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

testPipeArgs :: IO ()
testPipeArgs = do
  -- a |> b(c) -> b(a, c)
  let a = mkVar "a"
      b = mkVar "b"
      c = mkVar "c"
      b_c = mkCall b [c]
      input = mkProgram (mkPipe a b_c)
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = ECall b [a, c]

  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testPipeArgs failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

main :: IO ()
main = do
  putStrLn "Running tests..."
  testPipeSimple
  testPipeArgs
  putStrLn "All tests passed!"