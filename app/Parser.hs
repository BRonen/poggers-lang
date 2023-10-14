module Parser (parse, Expr (..)) where

import Data.List.Split
import Lexer (Token (..))

data Expr
  = Assignment String Expr Expr
  | Call String [Expr]
  | Abs [String] Expr
  | Numeric Integer
  | Text String
  | Tuple [Expr]
  deriving (Show)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

parseTuple :: [Token] -> Expr
parseTuple tokens = Tuple $ exprs
  where
    exprs = map (\x -> fst $ parse x) $ splitOn [Comma] tokens

parseArguments :: [Expr] -> [Token] -> ([Expr], [Token])
parseArguments acc [] = (acc, [])
parseArguments acc tokens@(Comma : _) = (acc, tokens)
parseArguments acc tokens@(RParen : _) = (acc, tokens)
parseArguments acc tokens@(BreakExpr : _) = (acc, tokens)
parseArguments acc tokens = (acc ++ [f] ++ f', s')
  where
    (f', s') = parseArguments [] s
    (f, s) = parse tokens

parseParameters :: [Token] -> [String]
parseParameters tokens = map f $ filter (/= Comma) tokens
  where
    f = \token -> case token of
      LiteralToken name -> name
      _ -> error $ show ("Invalid parameter", token)

getScopeTokens :: [Token] -> Int -> [Token]
getScopeTokens (token : tokens) currentLevel
  | currentLevel == 0 = []
  | token == LParen = token : getScopeTokens tokens (currentLevel + 1)
  | token == RParen = token : getScopeTokens tokens (currentLevel - 1)
  | otherwise = token : getScopeTokens tokens currentLevel
getScopeTokens tokens currentLevel
  | currentLevel == 0 = tokens
  | otherwise = []

parse :: [Token] -> (Expr, [Token])
parse [] = error "Empty source"
parse (token : tokens) = case token of
  BreakExpr -> parse tokens
  TextToken content -> (Text content, tokens)
  NumericToken value -> (Numeric value, tokens)
  LiteralToken name -> (Call name args, s)
    where
      (args, s) = parseArguments [] tokens
  Let -> (Assignment name f f', s')
    where
      (f', s') = parse s
      (f, s) = parse $ drop 2 tokens
      name = case head tokens of
        LiteralToken name' -> name'
        _ -> error $ show ("Invalid variable name", tokens)
  LParen -> case safeHead remainingTokens of
    Just FatArrow -> (Abs params body, remainingTokens')
      where
        params = parseParameters $ init scopeTokens
        (body, remainingTokens') = parse $ tail remainingTokens
    Nothing -> (parseTuple scopeTokens, [])
    _ -> (parseTuple scopeTokens, remainingTokens)
    where
      remainingTokens = drop (length scopeTokens) tokens
      scopeTokens = getScopeTokens tokens 1
  _ -> error $ show ("Trying to parse an invalid token", token)