module Lexer (tokenize, Token (..)) where

-- TODO: create a different literal for each type
data Token = Literal String | Plus | Minus | Slash | Star | Let | Equal | LParen | RParen | BreakExpr deriving (Show, Eq)

especialCharacters :: [Char]
especialCharacters = ['(', ')', ';', '+', '-', '/', '*']

-- TODO: Remove "++ ([acc])" and "++ ([[x]])"
getWordsFromSource :: [Char] -> [String] -> String -> [String]
getWordsFromSource acc wordsAcc [] = (wordsAcc ++ ([acc]))
getWordsFromSource acc wordsAcc (x:xs)
    | elem x especialCharacters = getWordsFromSource [] (wordsAcc ++ ([acc]) ++ ([[x]])) xs
    | x == ' ' || x == '\n' = getWordsFromSource [] (wordsAcc ++ ([acc])) xs
    | otherwise = getWordsFromSource (acc++([x])) wordsAcc xs

getTokenByWord :: [Char] -> Token
getTokenByWord word = case word of
    "let" -> Let
    "="   -> Equal
    "("   -> LParen
    ")"   -> RParen
    ";"   -> BreakExpr
    "+"   -> Plus
    "-"   -> Minus
    "/"   -> Slash
    "*"   -> Star
    _     -> Literal word

tokenize :: String -> [Token]
tokenize source = filter (\s -> s /= (Literal "")) $ map getTokenByWord $ getWordsFromSource [] [] source