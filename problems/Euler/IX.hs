module IX where

problem :: [Int]
problem = [ a*b*(1000-a-b)|a <-[1..1000],b <- [1..(1000-a)],(1000 - a -b)*(1000-a-b) == (a * a) + (b * b)]

main :: IO ()
main = print $ head $ problem
