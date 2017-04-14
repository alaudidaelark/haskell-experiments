module Main where

import qualified Control.Monad.State as S
import qualified Data.IntMap as Im

allMemoCollatzLen :: Int -> S.State (Im.IntMap Int) (Int,Int)
allMemoCollatzLen 1 = return $! (1,1)
allMemoCollatzLen n = do cache <- S.get
                         let val = Im.lookup n cache
                         case val of Just x -> do return $! (x,n)
                                     Nothing -> do S.put cumustate
                                                   return $! (colval,n)
                                                   where colnext = if (n `rem` 2) == 0 then (S.runState (allMemoCollatzLen $ n `div` 2) cache)
                                                                   else (S.runState (allMemoCollatzLen $ 3*n+1) cache)
                                                         colval = 1+(fst.fst $ colnext)
                                                         colstate = snd colnext
                                                         valinserted = Im.insert n colval cache
                                                         cumustate = Im.union valinserted colstate

memoCollatzLen :: Int -> S.State (Im.IntMap Int) (Int,Int)
memoCollatzLen 1 = return $! (1,1)
memoCollatzLen n = do cache <- S.get
                      let val = Im.lookup n cache
                      case val of Just x -> do return $! (x,n)
                                  Nothing -> do S.put valinserted
                                                return $! (colval,n)
                                                where colnext = if (n `rem` 2) == 0 then (S.runState (memoCollatzLen $ n `div` 2) cache)
                                                                else (S.runState (memoCollatzLen $ 3*n+1) cache)
                                                      colval = 1+(fst.fst $ colnext)
                                                      colstate = snd colnext
                                                      valinserted = Im.insert n colval cache
                                                      --cumustate = Im.union valinserted colstate


collatzLength :: Int -> (Int,Int)
collatzLength 1 = (1,1)
collatzLength n = if (n `rem` 2)==0 then (1+(fst.collatzLength $ (n `div` 2)),n)
                  else (1+(fst.collatzLength $ (3*n+1)),n)

collatzChain :: Int -> IO ()
collatzChain 1 = print 1
collatzChain n = do putStr $ (show n)++"->"
                    case n `rem` 2 == 0 of 
                         True -> collatzChain (n `div` 2)
                         False -> collatzChain (3*n+1)

main :: IO ()
main = do --print $ maximum $ S.evalState (sequence $ map minMemoCollatzLen [water,(water-1)..1]) Im.empty
          --print $ maximum $ S.evalState (sequence $ map memoCollatzLen [water,(water-1)..1]) Im.empty
          --print $ maximum $ map collatzLength [water,water-1..1]
          print "Hello World"
          where water = 1000000
