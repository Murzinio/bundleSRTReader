module Main where

import System.Environment
import Data.List
import System.TimeIt
import qualified Data.Text as Txt

import Reader
import Parser
import Types

main :: IO()
main = do
    [mainPath] <- getArgs
    timeIt $ runOptimized mainPath

run :: FilePath -> IO()
run path = do
    contents <- readBundles path
    let parsed = parse $ contents
    let strings = map (map (map (Txt.unpack))) parsed
    let toFile = intercalate "\n" $ sort $ concat $ concat strings
    writeFile "test.txt" toFile

runOptimized :: FilePath -> IO()
runOptimized path = do
    paths <- getPaths path
    runOptimizedImp paths

runOptimizedImp :: Paths -> IO()
runOptimizedImp [] = return ()
runOptimizedImp tmp = do
    print $ stripPath $ head tmp
    content <- readBundle $ head tmp --TODO will need to adjust to single content
    let parsed = parse $ [content]
    let strings = map (map (map (Txt.unpack))) parsed
    let toFile = intercalate "\n" $ sort $ concat $ concat strings
    appendFile "testAppend.txt" toFile
    runOptimizedImp $ tail tmp

headContents :: [Content] -> Content
headContents (x:xs) = x