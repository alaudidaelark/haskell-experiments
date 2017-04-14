module Main where

import qualified Data.Bits as B
import qualified Data.List as L

match :: [String] -> [String] -> Bool
match f s = if lfil (f !! 0) == lfil (s !! 0) then True 
            else if lfil (f !! 0) == lemp (s !! 0) then True
	    else False
            where lfil x= length $ (filter (=='1')) x
	          lemp x = length $ (filter (=='0')) x
	          

testCase :: Int -> IO ()
testCase caseNo = do indata <- getLine
                     let [devices,pins] = map read (words indata)::[Int]
		     pinData <- getLine
		     devData <- getLine
		     let switchInit = L.transpose.words $ pinData
		     let devpinOrder = L.transpose.words $ devData
                     putStr $ "Case #"++show caseNo++": "
		     print $ match switchInit devpinOrder
		                          
main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
	  mapM_ (\x -> testCase x) [1..testCaseInt]
