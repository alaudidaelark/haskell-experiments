module Blore (main) where

import           Control.Applicative
import           Control.Monad.ST
-- import           Control.Monad.State
import           Data.Bits
import           Data.STRef

testRef :: STRef s Integer -> [Int] -> ST s (Maybe Int)
testRef _ [] = return empty
testRef r (n:ks) = do v <- readSTRef r
                      let dup = testBit v n
                      let sbit = setBit v n
                      if dup
                        then return (pure n)
                        else do writeSTRef r sbit
                                testRef r ks

repBitStRef :: [Int] -> Maybe Int
repBitStRef xs = runST $ do bt <- newSTRef (0::Integer)
                            testRef bt xs

maxProfit :: [Int] -> Int
maxProfit ls =  maxp - minp
                where maxmin (m,n) (x:xs) = maxmin (max m x,min n x) xs
                      maxmin (m,n) [] = (m,n)
                      (maxp,minp) = maxmin (0,maxBound) ls

rep :: Int -> [Int] -> Int
rep n xs = sum xs  - div (n * (n+1)) 2

repBit :: Int -> [Int] -> Int
repBit n xs = nbit n `xor` xnbit xs
            where nbit k= foldr xor zeroBits [1..k]
                  xnbit = foldr xor zeroBits


-- bitState :: [Int] -> State Integer (Maybe Int)
-- bitState [] = return empty
-- bitState (n:xs) = do b <- get
--                      let dup = testBit b n
--                      let sbit = setBit b n
--                      return $ if dup
--                                 then pure n
--                                 else evalState (bitState xs) sbit
--
-- repBitState :: [Int] -> Maybe Int
-- repBitState xs  = evalState  (bitState xs) 0

main :: IO ()
main = do putStrLn $ "maxprofit for "++show input1list ++" : "++ show (maxProfit [1,2,3,4,5,6,7,8])
          -- putStrLn $ "duplicate using bitsettest "++show input2list ++" : "++ show (repBitState input2list)
          putStrLn $ "duplicate using bitsettest ref "++show input2list ++" : "++ show (repBitStRef input2list)
          putStrLn $ "duplicate using bitxor "++show input2list++" : " ++ show (repBit nvalue input2list)
          putStrLn $ "duplicate using sum "++show input2list ++" : "++ show (rep nvalue input2list)
    where input1list = [1,2,3,4,5,6,7,8]::[Integer]
          input2list = [1,2,3,4,5,5,6,7,8]::[Int]
          nvalue = 8
