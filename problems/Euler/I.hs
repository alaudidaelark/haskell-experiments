module I (naturalNumLessThan,numbersDivby3or5) where

naturalNumLessThan :: Int -> [Int]
naturalNumLessThan n = [1..n - 1]

numbersDivby3or5 :: Int -> Bool
numbersDivby3or5 n = if ((n `mod` 3) == 0) || ((n `mod` 5) == 0) then True else False

problem :: Int -> Int
problem n = sum $ filter numbersDivby3or5 $ naturalNumLessThan n

main :: IO ()
main = print $ problem 1000
