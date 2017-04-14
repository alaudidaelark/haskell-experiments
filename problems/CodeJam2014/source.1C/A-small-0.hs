module Main where

import Data.Ratio

denoPower2Test :: Integer -> Bool
denoPower2Test 1 = True
denoPower2Test n = even n && denoPower2Test (n `div` 2)

elfOrNot :: Integer -> Integer -> Maybe Int
elfOrNot num den 
    |num > den = Nothing
    |denoPower2Test den = Just $ generation num den
    |otherwise = Nothing
     
generation :: Integer -> Integer -> Int
generation elfact limit = if (limit `div` 2) <= elfact
                          then 1
			  else 1+generation elfact (limit `div` 2)

program :: Ratio Integer -> String
program inp = maybe "Impossible" show (elfOrNot (numerator inp) (denominator inp))

testCase :: Int -> IO ()
testCase caseNo = do inp <- getLine
                     let (num,den) = span (/= '/') inp
		     let numer = read num::Integer
		     let deno = read.tail $ den::Integer
		     let elfratio = numer % deno
                     putStrLn $ "Case #"++show caseNo++": "++program elfratio
		                          
main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
	  mapM_ testCase [1..testCaseInt]
