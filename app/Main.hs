module Main where

-- import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
import Eval (eval)

--import LuaPrinter (printToLua)

source :: String
source =
  unlines
    [ "let a = + 1 2;",
      "+ a 4"
    ]

main :: IO ()
main = do
  -- putStrLn "\n---------------\n"
  -- print $ tokenize "let a = fn x y => + (x) (y);\nlet c = (a 1 2);\nprint c"
  -- print '\n'
  -- print $ parse $ tokenize "let a = fn x y => + (x) (y);\nlet c = a 1 2 3 (b 1 2);\nprint c"
  -- print "\n lua:"
  -- print $ printToLua f
  print f
  print "\n evaluation:"
  result <- eval f
  print "\n evaluation return:"
  print result
  where
    (f, _) = parse $ tokenize "let a = fn x y => + x y;\nlet b = print (* 3 (a 1 2)); print b \"wasdwasd\""
