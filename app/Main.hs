module Main where

import MyLib qualified (someFunc)
import Vial.AST qualified as AST
import Vial.TypedAST qualified as TyAST

main :: IO ()
main = do
  -- Create basic metadata for demonstration
  let astSpan = AST.Span 0 10
  let meta = AST.Metadata astSpan "example.vl" []

  -- Create a simple type: Int
  let intType = AST.Type meta (AST.TyCon "Int" [])

  -- Create a typed integer expression: 42
  let typedIntExpr = TyAST.TypedExpr meta intType (TyAST.TELit (AST.LInt 42))

  putStrLn $ "Created TypedExpr: " ++ show typedIntExpr
  putStrLn $ "Type information: " ++ show (TyAST.typeOf typedIntExpr)

  -- Create another type: String
  let stringType = AST.Type meta (AST.TyCon "String" [])

  let typedStrExpr = TyAST.TypedExpr meta stringType (TyAST.TELit (AST.LString "Hello"))

  putStrLn $ "Created another TypedExpr: " ++ show typedStrExpr
  putStrLn $ "Type information: " ++ show (TyAST.typeOf typedStrExpr)

  let untypedInt = TyAST.untypeExpr typedIntExpr
  putStrLn $ "Untyped version of first expr: " ++ show untypedInt

  MyLib.someFunc
