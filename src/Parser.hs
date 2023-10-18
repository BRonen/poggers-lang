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
safeHead (a : _) = Just a

parseTuple :: [Token] -> Expr
parseTuple tokens = Tuple exprs
  where
    exprs = map (fst . parse) $ splitOn [Comma] tokens

parseArguments :: [Expr] -> [Token] -> ([Expr], [Token])
parseArguments acc [] = (acc, [])
parseArguments acc tokens@(Comma : _) = (acc, tokens)
parseArguments acc tokens@(RBracket : _) = (acc, tokens)
parseArguments acc tokens@(SemiColon : _) = (acc, tokens)
parseArguments acc tokens = (acc ++ [f] ++ f', s')
  where
    (f', s') = parseArguments [] s
    (f, s) = parse tokens

parseParameters :: [Token] -> [String]
parseParameters tokens = map f $ filter (/= Comma) tokens
  where
    f token = case token of
      LiteralToken name -> name
      _ -> error $ show ("Invalid parameter", token)

getBracketTokens :: [Token] -> Int -> [Token]
getBracketTokens (token : tokens) currentLevel
  | currentLevel == 0 = []
  | token == LBracket = token : getBracketTokens tokens (currentLevel + 1)
  | token == RBracket = token : getBracketTokens tokens (currentLevel - 1)
  | otherwise = token : getBracketTokens tokens currentLevel
getBracketTokens tokens currentLevel
  | currentLevel == 0 = tokens
  | otherwise = []

getParenTokens :: [Token] -> Int -> [Token]
getParenTokens (token : tokens) currentLevel
  | currentLevel == 0 = []
  | token == LParen = token : getParenTokens tokens (currentLevel + 1)
  | token == RParen = token : getParenTokens tokens (currentLevel - 1)
  | otherwise = token : getParenTokens tokens currentLevel
getParenTokens tokens currentLevel
  | currentLevel == 0 = tokens
  | otherwise = []

parse :: [Token] -> (Expr, [Token])
parse [] = error "Empty source"
parse (token : tokens) = case token of
  SemiColon -> parse tokens
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
  LBracket -> (parseTuple scopeTokens, remainingTokens)
    where
      remainingTokens = drop (length scopeTokens) tokens
      scopeTokens = getBracketTokens tokens 1
  LParen -> case safeHead s of
    Just FatArrow -> (Abs params body, s')
      where
        params = parseParameters $ init scopeTokens
        (body, s') = parse $ tail s
    _ -> (f, s)
    where
      (f, _) = parse $ init scopeTokens
      s = drop (length scopeTokens) tokens
      scopeTokens = getParenTokens tokens 1
  _ -> error $ show ("Trying to parse an invalid token", token)