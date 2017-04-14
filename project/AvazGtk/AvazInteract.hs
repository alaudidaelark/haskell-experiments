module Main where


import qualified Data.Map as M
import qualified Data.ByteString.Char8 as DCh8
import System.IO
import NGram
import Control.Monad

ngmregexinc = DCh8.pack "(\\d*)\\t(.*)\\t(.*)"

ngmFile = "avazlemma/uwcatfilteredstemmedngram7.txt"

appendTo x y = (head x):y

ngIns el m = M.insertWith appendTo (first el) [(second el,count el)] m

ngMap = do ng <- matchedNG ngmFile ngmregexinc
           return $ foldr ngIns M.empty ng

respondTo mng x = M.findWithDefault [] x mng

prompt ng = do putStr "prompt>"
               input <- getLine
	       let out = respondTo ng input
               putStrLn $ "No of Predictions :"++(show $ length out)++"\n"++(show out)

main :: IO ()
main = do ng <- ngMap
          hSetBuffering stdout NoBuffering
	  forever $ prompt ng
