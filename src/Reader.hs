module Reader(
    readBundles,
    readBundle,
    getPaths,
    stripPath)
where

import Data.List
import qualified Data.Text.IO as TxtIO
import System.FilePath.Find as FPF
import qualified Data.Text as Txt
import Data.Char

stripPath :: FilePath -> FilePath
stripPath p = takeWhile (/= '\n') p

readBundles :: FilePath -> IO [Txt.Text]
readBundles mainPath = do
    paths <- getPaths mainPath
    contents <- mapM (\x -> readBundle x) paths
    return $ contents

readBundle :: FilePath -> IO Txt.Text
readBundle path = do
    let filtered = stripPath path
    content <- TxtIO.readFile filtered
    if Txt.isInfixOf (Txt.pack ".srt") content
        then return content
        else return $ Txt.pack ""

getPaths :: FilePath -> IO [FilePath]
getPaths mainPath = do
    raw <- FPF.find always (extension ==? ".bundle") mainPath
    let filtered = filter (\x -> not $ isInfixOf "MODS" (map toUpper x)) raw
    return $ map (\x -> x ++ "\n") filtered