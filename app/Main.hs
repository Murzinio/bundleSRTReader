module Main where

import System.Environment
import Data.List
import Data.List.Split
import System.TimeIt
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIO
import qualified Data.Set as Set
import System.IO

import Reader
import Parser

main :: IO()
main = do
    [mainPath, outputName] <- getArgs
    writeFile outputName "" -- to test outputName
    putStrLn "Parsing bundles..."
    timeIt $ run mainPath outputName
    putStrLn "Cleaning output..."
    cleanOut outputName
    putStrLn "Done."
    hFlush stdout

cleanOut :: FilePath -> IO()
cleanOut outputName =
    do
        outputContent <- TxtIO.readFile outputName
        let splitted = Txt.splitOn (Txt.pack "\n") outputContent
        Txt.length outputContent `seq` return () -- force eval
        let output = Txt.intercalate (Txt.pack "\n") $ sort $ removeDup $ splitted
        TxtIO.writeFile outputName output

removeDup :: Ord a => [a] -> [a]
removeDup = Set.toList . Set.fromList

run :: FilePath -> String -> IO()
run path outputName = do
    paths <- getPaths path
    runImp outputName paths 0

runImp :: String -> [FilePath] -> Int -> IO()
runImp _ [] _ = return ()
runImp oName tmp count =
    do
        putStrLn $ (show count) ++ "\\" ++ (show $ length tmp) ++ " " ++ (stripPath $ head tmp)
        hFlush stdout
        content <- readBundle $ head tmp
        let parsed = parse $ content
        appendFile oName $ Txt.unpack parsed
        runImp oName (tail tmp) (count + 1)

headContents :: [Txt.Text] -> Txt.Text
headContents (x:xs) = x