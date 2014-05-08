module WordCount (wordCount) where

import Data.List(group, sort)
import Data.Map(Map, fromList)
import Data.Char(isAlphaNum, isSpace, toLower)

wordCount :: String -> Map String Int
wordCount phrase = fromList $ map (\xs -> (head xs, length xs)) . group . sort . tokenize $ phrase

tokenize :: String -> [String]
tokenize = words . map toLower . replace

replace :: String -> String
replace xs = [if isAllowed x then x else ' ' | x <- xs]

isAllowed :: Char -> Bool
isAllowed c = isAlphaNum c || isSpace c
