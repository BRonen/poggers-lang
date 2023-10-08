module Parser (parse, Expr (..)) where

import Lexer (Token (..))

data Expr
  = Assignment String Expr Expr
  | Call String [Expr]
  | Abs [String] Expr
  | Print [Expr]
  | Var String
  | Numeric Integer
  | Text String
  | EOF
  deriving (Show)

parseValue :: Token -> [Token] -> (Expr, [Token])
parseValue (TextToken value) tokens = (Text value, tokens)
parseValue (NumericToken value) tokens = (Numeric value, tokens)
parseValue (LiteralToken value) tokens = (Var value, tokens)
parseValue token _ = error $ show ("Invalid token passed to value parser", token)

parseArguments :: [String] -> [Token] -> ([String], [Token])
parseArguments acc ((LiteralToken argument) : tokens) = (argument : argumentsAcc, remainingTokens)
  where
    (argumentsAcc, remainingTokens) = parseArguments acc tokens
parseArguments acc (FatArrow : tokens) = (acc, tokens)
parseArguments _ tokens = error $ show ("Invalid function definition", tokens)

parseFunction :: [Token] -> (Expr, [Token])
parseFunction tokens = (Abs arguments body, remainingTokens')
  where
    (body, remainingTokens') = parse remainingTokens
    (arguments, remainingTokens) = parseArguments [] tokens

parseVariable :: [Token] -> (Expr, [Token])
parseVariable ((LiteralToken name) : Equal : tokens) = (Assignment name expr expr', remainingTokens')
  where
    (expr', remainingTokens') = parse remainingTokens
    (expr, remainingTokens) = parse tokens
parseVariable tokens = error $ show ("Invalid variable definition", tokens)

parseParameters :: [Expr] -> [Token] -> ([Expr], [Token])
parseParameters acc [] = (acc, [])
parseParameters acc tokens@(RParen : _) = (acc, tokens)
parseParameters acc tokens@(BreakExpr : _) = (acc, tokens)
parseParameters acc tokens = (acc ++ [f] ++ f', s')
  where
    (f', s') = parseParameters [] s
    (f, s) = parse tokens

parseCall :: [Token] -> (Expr, [Token])
parseCall ((LiteralToken name) : tokens) = (f, remainingTokens)
  where
    f = Call name args
    (args, remainingTokens) = parseParameters [] tokens
parseCall tokens = error $ show ("invalid call", tokens)

parsePrint :: [Token] -> (Expr, [Token])
parsePrint tokens = (Print f, s)
  where
    (f, s) = parseParameters [] tokens

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
parse [] = (EOF, [])
parse (token : tokens) = case token of
  Let -> parseVariable tokens
  FunctionDef -> parseFunction tokens
  PrintToken -> parsePrint tokens
  LParen -> (f, s)
    where
      (f, _) = parse scopeTokens
      s = drop (length scopeTokens) tokens
      scopeTokens = getScopeTokens tokens 1
  TextToken _ -> parseValue token tokens
  LiteralToken _ -> parseCall $ token : tokens
  NumericToken _ -> parseValue token tokens
  BreakExpr -> parse tokens
  _ -> error $ show ("Trying to parse an invalid token", token)