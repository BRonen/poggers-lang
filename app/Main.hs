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
  print (tokens)
  print (f, s)
  print "\n\n"
  print (tokens')
  print (f', s')
  print "\n\n"
  print (tokens'')
  print (f'', s'')

  r <- eval f''
  print r
  {-
  print "\n evaluation:"
    result <- eval f
    print "\n evaluation return:"
    print result
  -}
  where
    (f, s) = parse tokens
    tokens = tokenize "(1, 2, 3, \"hello\") a b c"
    (f', s') = parse tokens'
    tokens' = tokenize "print \"c\" 2 3"
    (f'', s'') = parse tokens''
    tokens'' = tokenize "let x = (a, b) => print (b, a); x \"hello\" - 5 3"
