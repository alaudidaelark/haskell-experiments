module Main where

import Data.List

program :: [String] -> String
program = show.validTrain.sort

sameCount :: [String] -> [Int]
sameCount (x:y:xs) = if last x == head x && head x == head y && head y == last y
                     then 2:sameCount (y:xs)
		     else [0]

validTrain :: [String] -> Bool
validTrain [] = True
validTrain (_:[]) = True
validTrain (x:y:[]) = last x == head y || succ (last x) == head y
validTrain (x:y:xs) = (last x == head y || succ (last x) == head y) && validTrain (y:xs)

testCase :: Int -> IO ()
testCase caseNo = do _ <- getLine
                     inpWords <- getLine
		     let input = words inpWords
                     putStrLn $ "Case #"++show caseNo++": "++program input
		                          
main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
	  mapM_ testCase [1..testCaseInt]
