module Reader(readBundles) where

import Data.List
import qualified Data.Text.IO as TxtIO
import System.FilePath.Find as FPF
import qualified Data.Text as Txt

import Types

srtPathPrefix :: Txt.Text
srtPathPrefix = Txt.pack "environment"

readBundles :: FilePath -> IO [Content]
readBundles mainPath = do
    paths <- getPaths mainPath
    let filtered = map (takeWhile (/= '\n')) paths 
    contents <- mapM (\x -> readBundle x) filtered
    return $ contents

readBundle :: FilePath -> IO Txt.Text
readBundle path = do
    content <- TxtIO.readFile path
    if Txt.isInfixOf (srtPathPrefix) content
        then return content
        else return $ Txt.pack ""

getPaths :: FilePath -> IO Paths
getPaths mainPath = do
    raw <- FPF.find always (extension ==? ".bundle") mainPath
    let filtered = filter (\x -> isInfixOf "blob" x) $ filter (\x -> isInfixOf "mods" x == False) raw
    return $ map (\x -> x ++ "\n") filtered