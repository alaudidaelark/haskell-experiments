module VI (sumOfSquare,squareOfSum) where

sumOfSquare :: Int -> Int
sumOfSquare n = sum $ map (\x->x*x) [1..n]

squareOfSum :: Int -> Int
squareOfSum n= (\x -> x*x) $ sum [1..n]

problem :: Int -> Int
problem n = abs $ (sumOfSquare n) - (squareOfSum n)

main :: IO ()
main = print $ problem 100
