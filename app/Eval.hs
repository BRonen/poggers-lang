module Eval (eval) where

import Data.Map as Map
import Parser (Expr (..))

data Value = ENumber Integer
  | EText String
  | EApplication Context [String] Expr
  | ETuple [Value]
  deriving (Show)

type Context = Map String Value

checkBuiltInValues :: String -> [Expr] -> Context -> IO Value
checkBuiltInValues name args ctx = case name of
  "+" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (ENumber v, ENumber v') -> return $ ENumber $ v + v'
        _ -> error "Sum with invalid params"
  "-" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (ENumber v, ENumber v') -> return $ ENumber $ v - v'
        _ -> error "Sub with invalid params"
  "*" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (ENumber v, ENumber v') -> return $ ENumber $ v * v'
        _ -> error "Mul with invalid params"
  "/" -> do
    f <- evalExpr ctx $ head args
    s <- evalExpr ctx $ head $ tail args

    case (f, s) of
        (ENumber v, ENumber v') -> return $ ENumber $ v `div` v'
        _ -> error "Div with invalid params"
  "print" -> do
    s <- sequence args'
    print s
    head args'
    where
      args' = Prelude.map (evalExpr ctx) args
  "fst" -> case head args of
      Tuple values -> evalExpr ctx $ head values
      _ -> error $ show ("Fst must be called with a tuple")
  "snd" -> case head args of
      Tuple values -> evalExpr ctx $ head $ tail values
      _ -> error $ show ("Snd must be called with a tuple")
  _ -> error $ show ("Reference not found", name)

evalExpr :: Context -> Expr -> IO Value
evalExpr ctx expr = case expr of
  Assignment name value next -> do
    expr' <- evalExpr ctx value
    let customCtx = Map.insert name expr' ctx
    evalExpr customCtx next
  Call name args -> case Map.lookup name ctx of
    Just (ENumber value) -> return $ ENumber value
    Just (EText value) -> return $ EText value
    Just (ETuple values) -> return $ ETuple values
    Just (EApplication ctx' args' expr') -> do
      args'' <- mapM (evalExpr ctx) args
      let args''' = zip args' args''
      let ctx'' = Prelude.foldl (flip $ uncurry Map.insert) ctx' args'''
      evalExpr ctx'' expr'
    Nothing -> checkBuiltInValues name args ctx
  Numeric value -> return $ ENumber value
  Text value -> return $ EText value
  Abs args expr' -> return $ EApplication ctx args expr'
  Tuple values -> do
    values' <- sequence $ Prelude.map (evalExpr ctx) values
    return $ ETuple values'

eval :: Expr -> IO String
eval expr = do
  result <- evalExpr Map.empty expr
  return $ show result