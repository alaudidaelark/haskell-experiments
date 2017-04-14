module II (fibo) where

fibo :: Int -> Int
fibo n = case n of 1 -> 1
                   2 -> 2
                   _ -> (fibo (n-1)) + (fibo (n-2))

problem :: Int -> Int
problem n = sum $ filter even $ takeWhile (< n) (map fibo [1..])

main :: IO ()
main = print $ problem 4000000
