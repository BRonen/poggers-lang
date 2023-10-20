module Parser (parse, SyntaxTree, STree (..)) where

import Data.List.Split
import Lexer (Token (..))

data STree
  = Assignment String STree STree
  | Call String [STree]
  | ICall STree [STree]
  | Ref String
  | Abs [String] STree
  | Numeric Integer
  | Text String
  | Tuple [STree]
  deriving (Show)

type SyntaxTree = Either (String, [Token]) STree

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

parseText :: [Token] -> SyntaxTree
parseText ((TextToken token):tokens) = Right $ Text token
parseText tokens = Left ("Invalid text", tokens)

parseNumber :: [Token] -> SyntaxTree
parseNumber ((NumericToken token):tokens) = Right $ Numeric token
parseNumber tokens = Left ("Invalid number", tokens)

parseAssignment :: [Token] -> SyntaxTree
parseAssignment (Let : LiteralToken name : _ : tokens) = case (value, next, rest') of
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

parseParameters :: [String] -> [Token] -> ([String], [Token])
parseParameters acc (RParen : FatArrow : tokens) = (acc, FatArrow : tokens)
parseParameters acc ((LiteralToken value) : tokens) = parseParameters (acc ++ [value]) tokens
parseParameters acc [] = (acc, [])
parseParameters _ _ = ([], [])

parseAbstraction :: [Token] -> (SyntaxTree, [Token])
parseAbstraction (LParen:tokens) = case safeHead rest of
  Just FatArrow -> case body of
    Right body' -> (Right $ Abs params body', rest')
    Left _ -> (body, [])
    where
      (body, rest') = parse rest
  _ -> (Left ("Invalid function definition", []), [])
  where
    (params, rest) = parseParameters [] tokens
parseAbstraction tokens = (Left ("Invalid Abstraction", tokens), tokens)

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
parseCall abs tokens = case safeHead tokens of
  Just Exclamation -> (Right $ ICall abs args, rest')
    where
      (args, rest') = parseArguments [] tokens
  _ -> (Right abs, tokens)

parseIdentifier :: Token -> [Token] -> (SyntaxTree, [Token])
parseIdentifier (LiteralToken name) (Exclamation : tokens) =
  (Right $ Call name args, rest)
  where
    (args, rest) = parseArguments [] tokens
parseIdentifier (LiteralToken name) tokens = (Right $ Ref name, tokens)
parseIdentifier token tokens = (Left ("Invalid identifier", tokens), token : tokens)

parse :: [Token] -> (SyntaxTree, [Token])
parse [] = (Left ("Empty source", []), [])
parse (token : tokens) = case token of
  SemiColon -> parse tokens
  _ -> case (text, number, assignment, tuple, abs, identifier) of
    (Right _, _, _, _, _, _) -> (text, tokens)
    (_, Right _, _, _, _, _) -> (number, tokens)
    (_, _, Right _, _, _, _) -> (assignment, [])
    (_, _, _, Right _, _, _) -> (tuple, tupleRest)
    (_, _, _, _, Right _, _) -> case (abs, safeHead absRest) of
      (Right abs', Just Exclamation) -> parseCall abs' absRest
      _ -> (abs, absRest)
    (_, _, _, _, _, Left _) -> parse tokens
    (_, _, _, _, _, Right _) -> (identifier, identifierRest)
  where
    (identifier, identifierRest) = parseIdentifier token tokens
    (abs, absRest) = parseAbstraction (token : tokens)
    (tuple, tupleRest) = parseTuple [] tokens
    assignment = parseAssignment $ token : tokens
    number = parseNumber $ token : tokens
    text = parseText $ token : tokens