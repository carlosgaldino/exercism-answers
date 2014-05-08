module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

areaCode :: String -> String
areaCode = take 3

number :: String -> String
number xs
  | length xs' == 10 = xs'
  | length xs' == 11 && head xs' == '1' = tail xs'
  | otherwise = "0000000000"
  where xs' = filter isDigit xs

prettyPrint :: String -> String
prettyPrint xs = concat ["(", a, ") ", b, "-", c]
  where (a, b, c) = parts xs

parts :: String -> (String, String, String)
parts xs = (a, b, c)
  where (a, bc) = splitAt 3 $ number xs
        (b, c)  = splitAt 3 bc
