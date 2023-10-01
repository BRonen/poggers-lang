module LuaPrinter.LuaPrinter (printToLua) where

import Parser (Expr)

printToLua :: Expr -> IO()
printToLua _ = print("")