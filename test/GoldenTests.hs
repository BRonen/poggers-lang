module GoldenTests (tests) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Eval (eval)
import Lexer (tokenize)
import Parser (parse)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

tests :: IO TestTree
tests = do
  pogFiles <- findByExtension [".pog"] "./test/snapshots"
  return $ testGroup "Golden tests" $ map genGoldenTest pogFiles

genGoldenTest :: FilePath -> TestTree
genGoldenTest pogFile =
  goldenVsString
    (takeBaseName pogFile)
    goldenFile
    (generateOutput =<< contentPog)
  where
    contentPog = BL.readFile pogFile
    goldenFile = replaceExtension pogFile ".output"

generateOutput :: BL.ByteString -> IO BL.ByteString
generateOutput contentPog = do
  print lexerContent
  print parserContent
  case parserContent of
    Right content -> do
      evalContent <- eval content
      return $ BLU.fromString $
        concat
          [ show lexerContent,
            ['\n', '\n'],
            show parserContent,
            ['\n', '\n'],
            show evalContent
          ]
    _ -> return $ BLU.fromString "Invalid parsing"

  where
    (parserContent, _) = parse lexerContent
    lexerContent = tokenize $ BLU.toString contentPog