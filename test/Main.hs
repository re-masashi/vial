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

mkVariant :: String -> String -> [Expr] -> Expr
mkVariant enumName varName args = mkExpr (EVariant enumName varName args)

mkQuestion :: Expr -> Expr
mkQuestion e = mkExpr (EQuestion e)

mkComptime :: Expr -> Expr
mkComptime e = mkExpr (EComptime e)

mkConstDecl :: String -> Type -> Expr -> Decl
mkConstDecl name ty expr = Decl dummyMeta (DConst name ty expr)

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

testEnumVariant :: IO ()
testEnumVariant = do
  -- Test that enum variant expressions are preserved through desugaring
  let someValue = mkVar "value"
      variant = mkVariant "Option" "Some" [someValue]
      input = mkProgram variant
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = EVariant "Option" "Some" [someValue]

  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testEnumVariant failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

testEnumVariantNoArgs :: IO ()
testEnumVariantNoArgs = do
  -- Test enum variant with no arguments
  let variant = mkVariant "Option" "None" []
      input = mkProgram variant
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = EVariant "Option" "None" []

  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testEnumVariantNoArgs failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

testQuestionOperator :: IO ()
testQuestionOperator = do
  -- Test that ? operator expressions are preserved through desugaring
  let expr = mkVar "result"
      questionExpr = mkQuestion expr
      input = mkProgram questionExpr
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = EQuestion expr

  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testQuestionOperator failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

testComptimeExpression :: IO ()
testComptimeExpression = do
  -- Test that comptime expressions are preserved through desugaring
  let expr = mkVar "some_code"
      comptimeExpr = mkComptime expr
      input = mkProgram comptimeExpr
      outputProg = desugar input
      output = getExprFromProgram outputProg
      expectedKind = EComptime expr

  unless (exprKind output == expectedKind) $ do
    putStrLn $ "testComptimeExpression failed. Expected " ++ show expectedKind ++ ", got " ++ show (exprKind output)
    fail "Test failed"

testConstDeclaration :: IO ()
testConstDeclaration = do
  -- Test that const declarations are preserved through desugaring
  let value = mkVar "value"
      constDecl = mkConstDecl "MY_CONST" (Type dummyMeta (TyCon "Int" [])) value
      input = Program [] [constDecl]
      outputProg = desugar input
      output = case pDecls outputProg of
                 [Decl _ (DConst name ty expr)] -> (name, ty, expr)
                 _ -> error "Unexpected program structure"

  unless (output == ("MY_CONST", Type dummyMeta (TyCon "Int" []), value)) $ do
    putStrLn $ "testConstDeclaration failed. Expected const declaration, got " ++ show output
    fail "Test failed"

main :: IO ()
main = do
  putStrLn "Running tests..."
  testPipeSimple
  testPipeArgs
  testEnumVariant
  testEnumVariantNoArgs
  testQuestionOperator
  testComptimeExpression
  testConstDeclaration
  putStrLn "All tests passed!"