module V (nthpowerlessThan) where

import Data.List

import III (factors)

nthpowerlessThan :: Int -> Int -> Int
nthpowerlessThan n i= last $ takeWhile (<= n) (map (i ^) [1..])

peakPrimePowers :: Int -> [Int]
peakPrimePowers n = nub $ map (nthpowerlessThan n) (filter (\x -> factors x == []) [2..n])

problem :: Int -> Int
problem = product.peakPrimePowers

main :: IO ()
main = print $ problem 20
