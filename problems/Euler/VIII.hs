module VIII where

import Data.Char

elem5 :: [Int] -> [[Int]]
elem5 xs = case xs of a:b:c:d:e:x -> [a,b,c,d,e]:(elem5 (b:c:d:e:x))
                      _           -> []

problem :: String -> Int
problem out = maximum $ map product $ elem5 $ map digitToInt $ concat $ lines out

main :: IO ()
main = do 
     out <- readFile "VIII.in"
     print $ problem out
