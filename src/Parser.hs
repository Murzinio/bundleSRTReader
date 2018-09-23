module Parser(parse) where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import qualified Data.Text as Txt

import Types

srtPattern :: Txt.Text
srtPattern = Txt.pack "env[a-z_\\.]{1,255}srt"

matchSrt :: Content -> [[Content]]
matchSrt c = c =~ srtPattern

parse :: [Content] -> [[[Content]]]
parse contents =
    map (map (filter (\x -> contentLength x > 0))) matched
    where
        matched = map (matchSrt) contents

contentLength :: Content -> Int
contentLength c = contentLengthImp c 0

contentLengthImp :: Content -> Int -> Int
contentLengthImp c n
    | Txt.null c = n
    | otherwise = contentLengthImp (Txt.dropEnd 1 c) (n + 1)