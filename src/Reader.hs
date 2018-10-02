module Reader(
    readBundle,
    getPaths,
    stripPath)
where

import Data.List
import Data.Char
import System.FilePath.Find as FPF
import qualified Data.Text.IO as TxtIO
import qualified Data.Text as Txt

stripPath :: FilePath -> FilePath
stripPath p = takeWhile (/= '\n') p

-- reads bundle, returns empty Text if the bundle doesn't contain any srts
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
    -- skip mods folders
    let filtered = filter (\x -> not $ isInfixOf "MODS" (map toUpper x)) raw
    return $ map (\x -> x ++ "\n") filtered