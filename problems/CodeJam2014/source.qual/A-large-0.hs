module Main where

import Data.List

getTestCase :: IO [Int]
getTestCase = do firstAnsStr <- getLine
                 let nMaxShy = read (words firstAnsStr !! 0) :: Int
                 let shyList = map (read.(\x->[x])) (words firstAnsStr !! 1) :: [Int]
                 return $ shyList

putTestCase :: [Int] -> Int -> Int -> Int -> Int
putTestCase [] sl extraMen totalMen = extraMen
putTestCase (x:xs) sl extraMen totalMen = if (totalMen >= sl)
                                          then putTestCase xs (sl+1) extraMen (totalMen+x)
                                          else putTestCase xs (sl+1) (extraMen+sl-totalMen) (sl+x)

testCase :: Int -> IO ()
testCase caseNo = do indata <- getTestCase
                     --print indata
                     putStr $ "Case #"++show caseNo++": "
                     putStrLn.show $ putTestCase indata 0 0 0

main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
          mapM_ (\x -> testCase x) [1..testCaseInt]
