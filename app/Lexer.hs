module Lexer (tokenize, Token (..)) where

import Data.Char (isDigit)
import Text.Read (readMaybe)

data Token
  = TextToken String
  | LiteralToken String
  | NumericToken Integer
  | PrintToken
  | Let
  | FunctionDef
  | FatArrow
  | Plus
  | Minus
  | Slash
  | Star
  | Equal
  | LParen
  | RParen
  | BreakExpr
  deriving (Show, Eq)

especialCharacters :: [Char]
especialCharacters = ['(', ')', ';']

-- TODO: remove duplicated code
getWordsFromSource :: [Char] -> [String] -> String -> [String]
getWordsFromSource acc wordsAcc [] = wordsAcc ++ [acc]
getWordsFromSource [] wordsAcc (x : xs)
  | x `elem` especialCharacters = getWordsFromSource [] (wordsAcc ++ [[x]]) xs
  | x == ' ' || x == '\n' = getWordsFromSource [] wordsAcc xs
  | otherwise = getWordsFromSource [x] wordsAcc xs
getWordsFromSource acc wordsAcc (x : xs)
  | x `elem` especialCharacters = getWordsFromSource [] (wordsAcc ++ [acc] ++ [[x]]) xs
  | x == ' ' || x == '\n' = getWordsFromSource [] (wordsAcc ++ [acc]) xs
  | otherwise = getWordsFromSource (acc ++ [x]) wordsAcc xs

tokenizeLiteral :: String -> Token
tokenizeLiteral word
  | all isDigit word = case (readMaybe word :: Maybe Integer) of
      Just value -> NumericToken value
      Nothing -> LiteralToken word
  | head word == '"' && last word == '"' = TextToken $ tail $ init word
  | otherwise = LiteralToken word

getTokenByWord :: [Char] -> Token
getTokenByWord word = case word of
  "print" -> PrintToken
  "let" -> Let
  "fn" -> FunctionDef
  "=>" -> FatArrow
  "=" -> Equal
  "(" -> LParen
  ")" -> RParen
  ";" -> BreakExpr
  _ -> tokenizeLiteral word

tokenize :: String -> [Token]
tokenize source = map getTokenByWord $ getWordsFromSource [] [] source