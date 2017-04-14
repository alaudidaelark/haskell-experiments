module Main where

import qualified Data.ByteString.Char8 as Bs
import qualified Data.Maybe as Mb

integerToJumpable :: Integer -> String
integerToJumpable 0 = "yes"
integerToJumpable 1 = "yes"
integerToJumpable 3 = "yes"
integerToJumpable 6 = "yes"
integerToJumpable n = if any (\x -> even x && x `rem` 3 == 0) [n,n-1,n-3]
                      then "yes"
                      else "no"

main :: IO ()
main = do inputLine <- Bs.getLine
          let inputInteger =  Mb.maybe 0 fst $ Bs.readInteger inputLine
	  putStrLn $ integerToJumpable inputInteger
