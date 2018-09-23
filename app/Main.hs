module Main where

import System.Environment
import Data.List
import System.TimeIt
import qualified Data.Text as Txt

import Reader
import Parser
import Types

main :: IO()
main = timeIt run

run :: IO()
run = do
    [mainPath] <- getArgs
    contents <- readBundles mainPath
    let parsed = parse $ contents
    let strings = map (map (map (Txt.unpack))) parsed
    let toFile = intercalate "\n" $ sort $ concat $ concat strings
    writeFile "test.txt" toFile

headContents :: [Content] -> Content
headContents (x:xs) = x