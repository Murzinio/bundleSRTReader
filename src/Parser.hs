module Parser(parse) where

import Data.List
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import qualified Data.Text as Txt

filterStart :: Txt.Text -> Txt.Text
filterStart xs
    | Txt.length xs > 1 =
        if (Txt.isPrefixOf (Txt.pack "dlc\\bob\\data\\environment") $ xs)
            then xs
            else filterStart $ Txt.tail xs
    | otherwise = 
        if (Txt.isPrefixOf (Txt.pack "dlc\\bob\\data\\environment") $ xs)
            then xs
            else Txt.empty

filterEnd :: Txt.Text -> Txt.Text
filterEnd xs
    | Txt.length xs > 1 =
        if (Txt.isPrefixOf (Txt.pack "trs") $ xs)
            then xs
            else filterEnd $ Txt.tail xs
    | otherwise =
        if (Txt.isPrefixOf (Txt.pack "trs") $ xs)
            then xs
            else Txt.empty

splitSource :: Txt.Text -> [Txt.Text]
splitSource src = Txt.splitOn (Txt.pack "\0") src

parse :: Txt.Text -> Txt.Text
parse content = 
    let
        emptyFiltered = filter (/= Txt.empty) $ splitSource content
        envFiltered = map filterStart $ emptyFiltered
        srtFiltered = map filterEnd $ map Txt.reverse envFiltered
        emptyFilteredFinal = filter (/= Txt.empty) srtFiltered
    in
        Txt.intercalate (Txt.pack "\n") $ map Txt.reverse emptyFilteredFinal

contentLength :: Txt.Text -> Int
contentLength c = contentLengthImp c 0

contentLengthImp :: Txt.Text -> Int -> Int
contentLengthImp c n
    | Txt.null c = n
    | otherwise = contentLengthImp (Txt.dropEnd 1 c) (n + 1)