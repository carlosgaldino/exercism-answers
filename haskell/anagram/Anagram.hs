module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (isAnagram word) . filterCandidates word

isAnagram :: String -> String -> Bool
isAnagram w candidate = sort (lowerCase w) == sort (lowerCase candidate)

filterCandidates :: String -> [String] -> [String]
filterCandidates word = filter (isDifferentWord word)

isDifferentWord :: String -> String -> Bool
isDifferentWord a b = lowerCase a /= lowerCase b

lowerCase :: String -> String
lowerCase = map toLower
