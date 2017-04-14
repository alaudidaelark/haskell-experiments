{-# OPTIONS_GHC -O3 -with-rtsopts="-N4" #-}

module Main where

import Data.List
tuple_to_list lt = (fst lt) ++ (snd lt)
n_factors_co_unsorted n = length $ tuple_to_list (unzip [ (j, (div n j)) | j <- [i | i <-[1..truncate (sqrt (fromIntegral n))], (mod n i) == 0]] )
n_factors_co_fast n = 2 * (length $ [i | i <-[1..truncate (sqrt (fromIntegral n))], (mod n i) == 0])


trianglelist = map (\x-> x*(x+1) `div` 2) [1..]

main :: IO ()
main = print output
       where output = head $ dropWhile (\x -> (n_factors_co_fast x) < 500) trianglelist
