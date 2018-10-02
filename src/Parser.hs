module Parser(parse) where

import Data.List
import qualified Data.Text as Txt

prefixDLC :: String
prefixDLC = "dlc\\bob\\data\\environment"

prefixVanilla :: String
prefixVanilla = "environment\\"

postfix :: String
postfix = ".srt"

filterBundle :: String -> Txt.Text -> Txt.Text
filterBundle prefix xs
    | Txt.length xs > 1 =
        if (Txt.isPrefixOf (Txt.pack prefix) $ xs)
            then xs
            else filterBundle prefix $ Txt.tail xs
    | otherwise = 
        if (Txt.isPrefixOf (Txt.pack prefix) $ xs)
            then xs
            else Txt.empty

splitSource :: Txt.Text -> [Txt.Text]
splitSource src = Txt.splitOn (Txt.pack "\0") src

parse :: Txt.Text -> Txt.Text
parse content = 
    let
        emptyFiltered = filter (/= Txt.empty) $ splitSource content
        envFilteredVanilla = map (filterBundle prefixVanilla) emptyFiltered
        envFilteredDLC = map (filterBundle prefixDLC) emptyFiltered
        srtFilteredVRev = map (filterBundle $ reverse postfix) $ map Txt.reverse envFilteredVanilla
        srtFilteredDRev = map (filterBundle $ reverse postfix) $ map Txt.reverse envFilteredDLC
        emptyFilteredFinal = filter (/= Txt.empty) $ concat [srtFilteredVRev, srtFilteredDRev]
    in
        Txt.intercalate (Txt.pack "\n") $ map Txt.reverse emptyFilteredFinal