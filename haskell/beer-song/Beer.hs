module Beer (sing, verse) where

import Data.Char (toTitle)

sing :: Int -> Int -> String
sing start stop
  | stop > start = ""
  | otherwise = verse start ++ "\n" ++ sing (start - 1) stop

verse :: Int -> String
verse x = firstLine x ++ "\n" ++ secondLine x ++ "\n"

secondLine :: Int -> String
secondLine 0 = "Go to the store and buy some more, " ++ wall 99 ++ "."
secondLine x = "Take " ++ one x ++ " down and pass it around, " ++ wall (x - 1) ++ "."
  where one n = if n == 1 then "it" else "one"

firstLine :: Int -> String
firstLine x = titleize (wall x) ++ ", " ++ beer x ++ "."
  where titleize (c:cs) = toTitle c : cs

wall :: Int -> String
wall x = beer x ++ " on the wall"

beer :: Int -> String
beer x = bottles x ++ " of beer" where
  bottles n
    | n == 0    = "no more bottles"
    | n == 1    = "1 bottle"
    | otherwise = show n ++ " bottles"
