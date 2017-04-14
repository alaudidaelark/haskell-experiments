module Main where


import qualified Data.Map as M
import qualified Data.ByteString.Char8 as DCh8
import UWDictRe
import NGram
import Control.Monad

ngIns el m = M.insertWith (\x y-> (head x):y) (first el) [(second el,count el)] m

uwIns el m = M.insert (phrase el) (show (cid el)++show (parent el)) m

ngmregex = DCh8.pack "(\\d*)\\t(.*)\\t(.*)\\r"


ngMap = do ng <- matchedNG "w2_.txt" ngmregex
           return $ foldr ngIns M.empty ng

uwMap = do uw <- matchedUW
           return $ foldr uwIns M.empty uw

{-respondTo mng muw x = case word0 of "ng" -> M.findWithDefault "Not Found in NG" word1 mng
                                    "uw" -> M.findWithDefault "Not Found in UW" word1 muw
			            _    -> "Invalid Command"
                where word0 = (words x) !! 0
		      word1 = (words x) !! 1
--}

respondTo mng x = show $ M.findWithDefault [("Nothing",0)] x mng

main :: IO ()
main = do ng <- ngMap
	  putStr "Enter Something>"
	  forever $ (getLine>>=(\x -> putStrLn $ respondTo ng x))
