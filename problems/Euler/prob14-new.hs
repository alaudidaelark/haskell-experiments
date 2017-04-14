module Main where

import qualified Control.Monad.State as S
import qualified Data.IntMap as Im

collatzState :: Int -> S.State (Im.IntMap Int) Int
collatzState n = do cache <- S.get
                    let val = Im.lookup n cache
                    case val of Just x -> do return x
		                Nothing -> case n%2 == 0 of True -> return (1+ 
				                            False ->

main :: IO ()
main = do print "Hello World"
