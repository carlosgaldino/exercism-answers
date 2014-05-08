module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLetter)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor s
  | all isSpace s    = "Fine. Be that way!"
  | isYelling s      = "Woah, chill out!"
  | isSuffixOf "?" s = "Sure."
  | otherwise        = "Whatever."

isYelling :: String -> Bool
isYelling s = not (null letters) && all isUpper letters
  where letters = filter isLetter s
