module Main where

-- import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
-- import Evaluate (evaluate)

source :: String
source = unlines [
        "let a = + 1 2;",
        "+ a 4"
    ]

main :: IO ()
main = do
    print $ tokenize $ init source
    print $ parse $ tokenize $ init source
    putStrLn "\n---------------\n"
    print $ tokenize "let a = fn x y => + x y;\nlet c = a 1 2;\nprint c"
    print $ parse $ tokenize "let a = fn x y => + x y;\nlet c = a 1 2;\nprint c"
