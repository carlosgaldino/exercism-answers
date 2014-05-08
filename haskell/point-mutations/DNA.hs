module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum . map fromEnum $ zipWith (/=) xs ys
