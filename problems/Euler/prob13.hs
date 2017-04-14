module Main where

main :: IO ()
main = do prob13 <- readFile "prob13.in"
          putStrLn $ take 10 $ show $ sum $ (map read $ lines $ prob13::[Integer])
