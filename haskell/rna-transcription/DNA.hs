module DNA (toRNA) where

toRNA :: String -> String
toRNA dna = map translate dna

translate :: Char -> Char
translate 'A' = 'U'
translate 'T' = 'A'
translate 'C' = 'G'
translate 'G' = 'C'
