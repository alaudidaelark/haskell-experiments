-- Some chefs go for a tour lasting N days. They take packages of bread for food. Each package has K pieces of breads. On the ith day, they eat Ai pieces of bread.
-- Unfortunately, chefs are very lazy people, and they always forget to close the package of breads, so each day the last piece of bread becomes exposed to mold (a fungus), and is no longer suitable for eating. Such a bad piece is not eaten, and is instead thrown away.
-- Let us take an example. If K = 4 and N = 3, then A = {3, 1, 2}. Chefs have packages of bread each having 4 pieces of bread, and their travel lasts 3 days. In the first day, they must eat 3 pieces of bread. So they open new package of bread and eat 3 pieces. They forget to close the package, so the 4th piece becomes bad. In the next day, they want to eat one piece of bread. And in the first package we don't have any good pieces of bread left, so they open a new package of bread and eat one piece from that. On the 3rd day, they want to eat 2 pieces of bread. In the second package, we have three pieces, and one of them is bad; so we have 2 good pieces. They eat 2 pieces from this package. So they must buy 2 packages of bread.
-- Please help chefs in finding out the minimum number of packages of breads they should take with them on the tour.
-- Input
--
-- The first line of input contains a single integer T denoting the number of test cases.
-- The first line of each test contains two space separated integers N and K.
-- The next line of each test case contains N space separated integers denoting the number of pieces of bread the chefs want to eat each day.
-- Output
--
-- For each of the T test cases, output a single line - minimum number of packages of bread the chefs should take.
-- Constraints and Subtasks
--
-- 1 ≤ T ≤ 10
-- Subtask 1: 15 points
-- 1 ≤ N ≤ 100
-- 1 ≤ K ≤ 100
-- 1 ≤ Ai ≤ 100
-- Subtask 2: 25 points
-- 1 ≤ N ≤ 10^5
-- 1 ≤ K ≤ 10^6
-- 1 ≤ Ai ≤ 10^6
-- Subtask 3: 60 points
-- 1 ≤ N ≤ 10^5
-- 1 ≤ K ≤ 10^11
-- 1 ≤ Ai ≤ 10^6
-- Example
--
-- Input:
-- 3
-- 3 4
-- 3 1 2
-- 1 1
-- 1
-- 2 4
-- 8 8
--
-- Output:
-- 2
-- 1
-- 4
-- Explanation
--
-- Test case 1 has already been explained in the statement.
-- In test case 2, we have one day tour and packages with one piece each. In the first day, we have to eat one piece of bread, so we open a package and eat one piece. Tour ended, and our answer is 1.
-- In test case 3, we have a two days tour, and packages with 4 pieces of bread each. In the first day, we have to eat 8 pieces. We need to open two packages and eat all the pieces. In the second day, we have to eat 8 pieces again. We open two packages and eat all pieces. Tour ended. Answer is 4.
module Main where

import Control.Applicative
import Control.Monad
-- import Debug.Trace

eatBreads :: [Int] -> Int -> Int -> Int -> (Int,Int)
eatBreads [] _ leo pc = (pc,leo)
eatBreads (bte:od) bip leo pc = eatBreads od bip leftTom (pc+bpe)
-- eatBreads a@(bte:od) bip leo pc = trace (show a++show bip++show leo++show pc) $ eatBreads od bip leftTom (pc+bpe)
                                  where leot = (leo -1) - bte
                                        blte
                                          | leo == 0 || leo == 1 = bte
                                          | leot >= 0 = 0
                                          | otherwise = -leot
                                        bpe
                                          | blte == 0 = 0
                                          | mod blte bip == 0 = div blte bip
                                          | otherwise = div blte bip+1
                                        leftTom
                                          | leot >= 0 = leot
                                          | mod blte bip == 0 = 0
                                          | otherwise = bip - mod blte bip

runTests :: IO ()
runTests = do [_,k]<- map read . words <$> getLine
              a <- map read.words <$> getLine
              -- print (n::Int,k::Int,a::[Int])
              print.fst $ eatBreads a k 0 0

main :: IO ()
main = do noOfTests <- read <$> getLine
          replicateM_ noOfTests runTests
