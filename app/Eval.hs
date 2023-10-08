module Eval (eval) where

import Data.Map as Map
import Parser (Expr (..))

data Value = A Integer | B String | C Context [String] Expr deriving (Show)

type Context = Map String Value

checkBuiltInValues :: String -> [Expr] -> Context -> IO Value
checkBuiltInValues name args ctx = case name of
  "+" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (A v, A v') -> return $ A $ v + v'
        _ -> error "Sum with invalid params"
  "-" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (A v, A v') -> return $ A $ v - v'
        _ -> error "Sum with invalid params"
  "*" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (A v, A v') -> return $ A $ v * v'
        _ -> error "Sum with invalid params"
  "/" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (A v, A v') -> return $ A $ v `div` v'
        _ -> error "Sum with invalid params"
  _ -> error $ show ("Reference not found", name)

evalExpr :: Context -> Expr -> IO Value
evalExpr ctx expr = case expr of
  Assignment name value next -> do
    expr' <- evalExpr ctx value
    let customCtx = Map.insert name expr' ctx
    evalExpr customCtx next
  Call name args -> case Map.lookup name ctx of
    Just (A value) -> return $ A value
    Just (B value) -> return $ B value
    Just (C ctx' args' expr') -> do
      args'' <- mapM (evalExpr ctx) args
      let args''' = zip args' args''
      let ctx'' = Prelude.foldl (flip $ uncurry Map.insert) ctx' args'''
      evalExpr ctx'' expr'
    Nothing -> checkBuiltInValues name args ctx
  Var name -> case Map.lookup name ctx of
    Just value -> return value
    Nothing -> error $ show ("Reference not found", name)
  Numeric value -> return $ A value
  Text value -> return $ B value
  Print args -> do
    s <- sequence args'
    print s
    head args'
    where
      args' = Prelude.map (evalExpr ctx) args
  Abs args expr' -> return $ C ctx args expr'
  EOF -> error "Error on evalutation"

eval :: Expr -> IO String
eval expr = do
  result <- evalExpr Map.empty expr
  return $ show result