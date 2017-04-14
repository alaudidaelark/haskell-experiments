module Main where

import Control.Monad
import Data.List
--import Data.Maybe as Mb

distList :: String -> String -> [Int]
distList [] _ = []
distList _ [] = []
distList (t:tt) tc = count:distList tt (snd splitval)
                     where count = length (fst splitval)
		           splitval = span (==t) tc

minAction :: [[Int]] -> [Int] -> Int
minAction xs ys = sum (map (diff ys) xs)
                  where diff x y = sum (map abs (zipWith (-) x y))

prog :: [String] -> String -> Int
prog xs x = minAction (map (distList x) xs) (targetString xs x)

targetString :: [String] -> String -> [Int]
targetString xs y = map (\x -> fromInteger $ ceiling $ (toRational.sum $ x)/(toRational.length$ x)) (transpose (map (distList y) xs))
--targetString :: [String] -> String -> [Int]
--targetString xs y = map (\x -> sum x `div` length x) (transpose (map (distList y) xs))


same :: [String] -> Maybe String
same [] = Nothing
same (x:[]) = Just x
same (x:xs) = if x == head xs
              then same xs
              else Nothing

uniq :: String -> String
uniq [] = []
uniq (x:[]) = [x]
uniq (x:xs) = if x == head xs
              then uniq xs
	      else x:uniq xs

target :: [String] -> Maybe String
target = same.map uniq

--sureTarget :: [String] -> String
--sureTarget xs = Mb.fromMaybe (head xs) (same $ map nub xs)

program :: [String] -> String
program xs = case target xs of Nothing -> "Fegla Won"
                               Just x -> show $ prog xs x

testCase :: Int -> IO ()
testCase caseNo = do indata <- getLine
                     let noOfLines = read indata::Int
		     inStrings <- replicateM noOfLines getLine
                     putStr $ "Case #"++show caseNo++": "
		     putStrLn $ program inStrings

main :: IO ()
main = do testCaseStr <- getLine
          let testCaseInt = read testCaseStr::Int
          mapM_ testCase [1..testCaseInt]
