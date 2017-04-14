module VII (primes) where

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

problem :: Int -> Int
problem n = primes !! (n - 1)

main :: IO ()
main = print $ problem 10001
