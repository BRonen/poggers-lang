module LuaPrinter (printToLua) where

import Data.List (intercalate)
import Parser (Expr (..))

{-
    TODO: implement a flag context to know things like identation, when return and optimizations
-}

buildFunctionCall :: String -> [Expr] -> String
buildFunctionCall name args = case name of
  "+" -> concat ["(", concat $ buildSource [] $ head args, " + ", concat $ buildSource [] $ head $ init args, ")"]
  "-" -> concat ["(", concat $ buildSource [] $ head args, " - ", concat $ buildSource [] $ head $ init args, ")"]
  "*" -> concat ["(", concat $ buildSource [] $ head args, " * ", concat $ buildSource [] $ head $ init args, ")"]
  "/" -> concat ["(", concat $ buildSource [] $ head args, " / ", concat $ buildSource [] $ head $ init args, ")"]
  _ -> concat $ name : ["(", args', ")\n"]
  where
    args' = intercalate ", " $ concatMap (buildSource []) args

buildSource :: [String] -> Expr -> [String]
buildSource acc expr = case expr of
  Assignment name value next -> buildSource acc' next
    where
      acc' = buildSource (acc ++ ["\nlocal ", name, " = "]) value
  Abs args body -> buildSource acc' body ++ ["\nend\n"]
    where
      acc' = acc ++ ["function (", args', ")\n"]
      args' = intercalate ", " args
  Call name args -> acc ++ [buildFunctionCall name args]
  Numeric value -> acc ++ [show value]
  Text value -> acc ++ ["'" ++ value ++ "'"]
  Tuple values -> ["{", values'', "}"]
    where
      values'' = intercalate ", " $ concat values'
      values' = map (buildSource []) values

printToLua :: Expr -> String
printToLua expr = concat $ buildSource [] expr