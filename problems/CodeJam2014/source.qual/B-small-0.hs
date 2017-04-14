module Main where

import Data.List
--import qualified Data.Sequence as Sq

getTestCase :: IO [Int]
getTestCase = do firstAnsStr <- getLine
                 sndLine <- getLine
                 let shyList = map read (words sndLine) :: [Int]
                 --return $ (read firstAnsStr,shyList)
                 return $ shyList

--putTestCase :: [Int] -> Int
--putTestCase panList  0 = (sort panList) !! 0

chefMinTime :: [Int] -> Int -> Int
chefMinTime inputTask timeSpent = findMinTime (cleanIn inputTask) timeSpent

cleanIn :: [Int] -> [Int]
cleanIn = sortBy (flip compare) . filter (>0)


findMinTime :: [Int] -> Int -> Int
findMinTime [] timeSpent = timeSpent
findMinTime ax@(x:xs) timeSpent = if x <=3 then (x+timeSpent)
                                  else if splTime < noopTime then splTime
                                  else noopTime
                                  where   splTime = chefMinTime splTask (timeSpent+1) 
                                          splTask =  (x-d2:d2:xs)
                                          d2 = div x 2
                                          noopTime = chefMinTime noopTask (timeSpent+1)
                                          noopTask = map pred ax

{-
findMinTime :: [Int] -> Int -> Int
findMinTime [] timeSpent = timeSpent
findMinTime ax@(x:xs) timeSpent = if splTime < noopTime
                                  then splTime
                                  else noopTime
                                    where cleanIn = filter (>0) . sortBy (flip compare)
                                          splTime = findMinTime splTask (timeSpent+1) 
                                          splTask =  cleanIn (d2:x-d2:xs)
                                          d2 = div x 2
                                          noopTime = findMinTime noopTask (timeSpent+1)
                                          noopTask = cleanIn (map pred ax)
-}


testCase :: Int -> IO ()
testCase caseNo = do indata <- getTestCase
                     --print indata
                     let ans = (flip chefMinTime $ 0) $ indata
                     putStr$ "Case #"++show caseNo++": "
                     putStrLn.show $ ans

main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
          mapM_ (\x -> testCase x) [1..testCaseInt]
