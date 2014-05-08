module ETL (transform) where

import qualified Data.Map as M
import Data.Char (toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform = M.fromList . concat . map switchKey . M.toList

switchKey :: (Int, [String]) -> [(String, Int)]
switchKey (p, xs) = map ((flip (,) p) . map toLower) xs
