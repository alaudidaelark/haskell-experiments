module IV (threeDigit,prodThree,palin) where

import Control.Applicative

threeDigit :: [Int]
threeDigit = [999,998..100]

prodThree :: [Int]
prodThree = (*) <$> threeDigit <*> threeDigit

palin :: String -> Bool
palin x = if x == reverse x then True else False

problem :: Int
problem = maximum $ filter (palin.show) prodThree

main :: IO ()
main = print $ problem
