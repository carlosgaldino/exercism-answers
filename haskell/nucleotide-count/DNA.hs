module DNA (count, nucleotideCounts) where

import Data.Map (Map, fromList, insertWith)

count :: Char -> String -> Int
count c = if c `elem` "AGCTU"
    then length . filter (== c)
    else error ("invalid nucleotide '" ++ [c] ++ "'")

-- since this version is using `count` the string will be traversed for each nucleotide.
nucleotideCounts :: String -> Map Char Int
nucleotideCounts xs = fromList [(c, count c xs) | c <- "ACGT"]

-- in this version the string will be traversed just one time.
nucleotideCounts :: String -> Map Char Int
nucleotideCounts = foldl updateCounter counter
  where counter = fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)]

updateCounter :: Map Char Int -> Char -> Map Char Int
updateCounter m c = insertWith (+) c 1 m
