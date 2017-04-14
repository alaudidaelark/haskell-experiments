module III (upLimit,possFactor,factor,smallFactors,factors,primeFactors,largestPrimeFactor) where

import Data.List

upLimit :: Int -> Int
upLimit = floor.sqrt.fromIntegral

possFactor :: Int -> [Int]
possFactor n = [2..upLimit n]

factor :: Int -> Int -> Bool
factor n i = if (n `mod` i) == 0 then True else False

smallFactors :: Int -> [Int]
smallFactors n = filter (factor n) (possFactor n)

factors :: Int -> [Int]
factors n = nub $ (smallFactors n) ++ (map (div n) (reverse $ smallFactors n))

primeFactors :: Int -> [Int]
primeFactors n = filter (\x -> factors x == []) (factors n)

largestPrimeFactor :: Int -> Int
largestPrimeFactor = maximum.primeFactors

problem :: Int -> Int
problem = largestPrimeFactor

main :: IO ()
main = print $ problem 600851475143
