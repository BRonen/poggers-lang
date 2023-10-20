module Main (main) where

import Eval (eval)
import Lexer (tokenize)
-- import LuaPrinter (printToLua)
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  case length args of
    0 -> do
      print "Need to specify a poggers file path:"
      putStrLn "\t./poggerslang [file path]"
    _ -> do
      let filePath = head args
      fileContent <- readFile filePath

      let tokens = tokenize fileContent
      case parse tokens of
        (Right tree, rest) -> do
          result <- eval tree
          print $ show (result, rest)
        (Left err, rest) -> print $ show (err, rest)
  {-
    print $ tokenize "let apply = (op a b) => op! a b; let add = (a b) => +! a b; print! (apply! add 2 3)"
    case parse $ tokenize "let apply = (op a b) => op! a b; let add = (a b) => +! a b; print! apply! add 2 3" of
      (Right value, a) -> do
        print $ show (value, a)
        r <- eval value
        print r
      (Left a, _) -> error $ show a
    
    putStrLn "\n---------------\n"
    print $ tokenize "let a = (x y) => + (x) (y);\nlet c = (a 1 2);\nprint c"
    print '\n'
    print $ fst $ parse $ tokenize "let a = (x y) => + (x) (y);\nlet c = (a 1 2);\nprint c"
    print "\n lua:"
    --print $ printToLua f
    putStrLn "\n---------------\n"

    print tokens
    print (f, s)
    print "\n\n"
    print tokens'
    print (f', s')
    print "\n\n"
    print tokens''
    print (f'', s'')

    --print $ printToLua f''
    where
      {-
      print "\n evaluation:"
        result <- eval f
        print "\n evaluation return:"
        print result
      -}
      (f, s) = parse tokens
      tokens = tokenize "[1, 2, 3, \"hello\"] a b c"
      (f', s') = parse tokens'
      tokens' = tokenize "print \"c\" 2 3"
      (f'', s'') = parse tokens''
      tokens'' = tokenize "let z = 2; let y = (a, b) => b;let x = (a, b) => print [b, a]; x \"hello\" (- 10 (+ (y 1 z) (y 1 (* (z) 2))))"
  -}