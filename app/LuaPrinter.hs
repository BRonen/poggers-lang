module LuaPrinter (printToLua) where

import Data.List
import Parser (Expr (..))

{-
    TODO: implement a flag context to know things like identation, when return and optimizations
-}

buildFunctionCall :: String -> [Expr] -> String
buildFunctionCall name args = concat $ name : ["(", args',")\n"]
    where
        args' = intercalate ", " $ concatMap (buildSource []) args

buildSource :: [String] -> Expr -> [String]
buildSource acc expr = case expr of
    Assignment name value next -> buildSource acc' next
        where
            acc' = buildSource (acc ++ ["local ", name, " = "]) value
    Abs args body -> buildSource acc' body
        where
            acc' = acc ++ ["function (", args',")\n"]
            args' = intercalate ", " args
    Call name args -> acc ++ [buildFunctionCall name args]
    Numeric value -> acc ++ [show value]
    Text value -> acc ++ ["'", value, "'"]

printToLua :: Expr -> String
printToLua expr = concat $ buildSource [] expr