module GoldenTests(tests) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.Golden (goldenVsString, findByExtension)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.FilePath (takeBaseName, replaceExtension)

import Lexer ( tokenize )
import Parser ( parse )
import Eval ( eval )

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
        evalContent <- eval $ fst parserContent
        return $ BLU.fromString $ concat [
                show lexerContent,
                ['\n', '\n'],
                show parserContent,
                ['\n', '\n'],
                show evalContent
            ]
    where
        parserContent = parse lexerContent
        lexerContent = tokenize $ BLU.toString contentPog