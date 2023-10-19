module Parser (parse, SyntaxTree, STree (..)) where

import Data.List.Split
import Lexer (Token (..))

data STree
  = Assignment String STree STree
  | Call String [STree]
  | ICall STree [STree]
  | Abs [String] STree
  | Numeric Integer
  | Text String
  | Tuple [STree]
  deriving (Show)

type SyntaxTree = Either (String, [Token]) STree

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

parseAssignment :: [Token] -> SyntaxTree
parseAssignment (LiteralToken name : _ : tokens) = case (value, next, rest') of
  (Right value', Right next', []) -> Right $ Assignment name value' next'
  (Left _, _, []) -> value
  (_, Left _, []) -> next
  _ -> Left ("Invalid tokens found", rest')
  where
    (next, rest') = parse rest
    (value, rest) = parse tokens
parseAssignment tokens = Left ("Invalid variable definition", tokens)

parseTuple :: [STree] -> [Token] -> (SyntaxTree, [Token])
parseTuple values [] = (Left ("End of tuple not found", []), [])
parseTuple values (RBracket : tokens) = (Right $ Tuple values, tokens)
parseTuple values tokens = case value of
  Left _ -> (value, [])
  Right value' -> parseTuple (values ++ [value']) rest
  where
    (value, rest) = parse tokens

parseParameters :: [String] -> [Token] -> Either Bool ([String], [Token])
parseParameters acc (RParen : FatArrow : tokens) = Right (acc, tokens)
parseParameters acc ((LiteralToken value) : tokens) = parseParameters (acc ++ [value]) tokens
parseParameters acc _ = Left False

parseAbstraction :: [Token] -> Either Bool (SyntaxTree, [Token])
parseAbstraction tokens = case parameters of
  Right f -> case body of
    Right body' -> Right (Right $ Abs parameters' body', rest)
    l -> Right (l, [])
    where
      (body, rest) = parse tokens'
      (parameters', tokens') = f
  Left f -> Left f
  where
    parameters = parseParameters [] tokens

parseArguments :: [STree] -> [Token] -> ([STree], [Token])
parseArguments acc [] = (acc, [])
parseArguments acc (RParen : tokens) = (acc, tokens)
parseArguments acc (SemiColon : tokens) = (acc, tokens)
parseArguments acc (Comma : tokens) = (acc, tokens)
parseArguments acc tokens = case f of
  Right f' -> parseArguments (acc ++ [f']) rest
  Left _ -> (acc, rest)
  where
    (f, rest) = parse tokens

parseCall :: STree -> [Token] -> (SyntaxTree, [Token])
parseCall f tokens = case safeHead tokens of
  Just Exclamation -> (Right $ ICall f args, rest')
    where
      (args, rest') = parseArguments [] tokens
  _ -> (Right f, tokens)

parse :: [Token] -> (SyntaxTree, [Token])
parse [] = (Left ("Empty source", []), [])
parse (token : tokens) = case token of
  SemiColon -> parse tokens
  NumericToken value -> (Right $ Numeric value, tokens)
  TextToken value -> (Right $ Text value, tokens)
  Let -> (parseAssignment tokens, [])
  LBracket -> parseTuple [] tokens
  LParen -> case abs of
    Right (f, rest) -> case f of
        Right f' -> parseCall f' rest
        l -> (l, [])
    Left _ -> parse tokens
    where
    abs = parseAbstraction tokens
  LiteralToken name -> case safeHead tokens of
    Just Exclamation -> (Right $ Call name args, rest)
      where
        (args, rest) = parseArguments [] (tail tokens)
    _ -> (Right $ Call name [], tokens)
  _ -> (Left ("Trying to parse an invalid token", token : tokens), [])