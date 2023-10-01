module Parser (parse, Expr (..)) where

import Lexer (Token (..))

data Operator = Add | Sub | Div | Mul deriving (Show)
data Expr = Assignment String Expr | Operation Operator Expr Expr | Var String Expr | Call String [Expr] | Text String | Numeric Integer deriving (Show)

-- TODO: create a different literal for each type
parseLiteral :: String -> [Token] -> (Expr, [Token])
parseLiteral literal tokens
    | head literal == '"' && last literal == '"' = (Text (tail (init literal)), tokens)
    | otherwise = (Numeric (read literal :: Integer), tokens)

parseOperator :: Operator -> [Token] -> (Expr, [Token])
parseOperator op tokens = (Operation op f f', s')
    where
        (f', s') = parse s
        (f, s) = parse tokens

parse :: [Token] -> (Expr, [Token])
parse [] = (Text "Hello world", [])
parse (token:tokens) = case token of
    Plus  -> parseOperator Add tokens
    Minus  -> parseOperator Sub tokens
    Slash  -> parseOperator Div tokens
    Star  -> parseOperator Mul tokens
    Literal s  -> parseLiteral s tokens
    _     -> (Text "[Object object]", tokens)