module Eval (evaluate) where

import Parser (Expr)

evaluate :: Expr -> IO()
evaluate _ = print("")