module Main where

-- import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
-- import Evaluate (evaluate)

source :: String
source = unlines [
        "let x = 1;",
        "let y = + 2 3;",
        "print('wdawd');",
        "+ x y"
    ]

main :: IO ()
main = do
    -- print $ parse $ tokenize source
    print $ tokenize "+ 1 4"
    print $ parse $ tokenize "+ 1 4"
