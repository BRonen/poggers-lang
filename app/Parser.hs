module Parser (parse, Expr (..), Operator (..)) where

import Lexer (Token (..))

data Operator = Add
    | Sub
    | Div
    | Mul
        deriving (Show)
data Expr = Assignment String Expr Expr
    | Abs [String] Expr
    | Print [Expr]
    | Operation Operator Expr Expr
    | Var String
    | Call String [Expr]
    | Numeric Integer
    | Text String
    | Literal String
        deriving (Show)

parseValue :: Token -> [Token] -> (Expr, [Token])
parseValue (TextToken value) tokens = (Text value, tokens)
parseValue (NumericToken value) tokens = (Numeric value, tokens)
parseValue (LiteralToken value) tokens = (Literal value, tokens)
parseValue token _ = error $ show ("Invalid token passed to value parser", token)

parseOperator :: Operator -> [Token] -> (Expr, [Token])
parseOperator op tokens = (Operation op f f', s')
    where
        (f', s') = parse s
        (f, s) = parse tokens

parseArguments :: [String] -> [Token] -> ([String], [Token])
parseArguments acc ((LiteralToken argument):tokens) = ([argument] ++ argumentsAcc, remainingTokens)
    where (argumentsAcc, remainingTokens) = parseArguments acc tokens
parseArguments acc ((FatArrow):tokens) = (acc, tokens)
parseArguments _ tokens = error $ show ("Invalid function definition", tokens)

parseFunction :: [Token] -> (Expr, [Token])
parseFunction tokens = (Abs arguments body, remainingTokens')
    where
        (body, remainingTokens') = parse remainingTokens
        (arguments, remainingTokens) = parseArguments [] tokens

parseVariable :: [Token] -> (Expr, [Token])
parseVariable ((LiteralToken name):(Equal):tokens) = (Assignment name expr expr', remainingTokens')
    where
        (expr', remainingTokens')= parse remainingTokens
        (expr, remainingTokens) = parse tokens
parseVariable tokens = error $ show ("Invalid variable definition", tokens)

parseParameters :: [Expr] -> [Token] -> ([Expr], [Token])
parseParameters acc [] = (acc, [])
parseParameters acc ((RParen):tokens) = (acc, tokens)
parseParameters acc tokens = ((acc ++ [f] ++ f'), s')
    where
        (f', s') = parseParameters [] s
        (f, s) = parse(tokens)

parseCall :: [Token] -> (Expr, [Token])
parseCall ((LiteralToken name):tokens) = (f, remainingTokens)
    where
        f = (Call name args)
        (args, remainingTokens) = parseParameters [] tokens
parseCall tokens = error $ show ("invalid call", tokens)

parsePrint :: [Token] -> (Expr, [Token])
parsePrint tokens = (Print f, s)
    where
        (f, s) = parseParameters [] tokens

parse :: [Token] -> (Expr, [Token])
parse [] = (Var "Hello world", [])
parse (token:tokens) = case token of
    Let -> parseVariable tokens
    FunctionDef -> parseFunction tokens
    PrintToken -> parsePrint tokens
    LParen -> parseCall tokens
    Plus  -> parseOperator Add tokens
    Minus -> parseOperator Sub tokens
    Slash -> parseOperator Div tokens
    Star  -> parseOperator Mul tokens
    TextToken _    -> parseValue token tokens
    LiteralToken _ -> parseValue token tokens
    NumericToken _ -> parseValue token tokens
    BreakExpr -> parse tokens
    _   -> error $ show ("Trying to parse an invalid token", token)