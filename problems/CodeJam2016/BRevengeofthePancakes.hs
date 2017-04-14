import           Control.Monad.ST
import           Data.Bits
import           Data.List
import           Data.Maybe
import           Data.STRef

bitsToSet :: String -> [Int]
bitsToSet xs = map m $ filter f $ zip [k,k-1..0] xs
               where k = length xs -1
                     f (x,y) = y=='1'
                     m (x,_)= x

readBinStr :: (Bits b,Num b) => String -> b
readBinStr xs = foldl setBit zeroBits (bitsToSet xs)

cakeToBin :: String -> String
cakeToBin = map f
            where f '+' = '1'
                  f '-' = '0'

-- revPanCake :: String -> Integer
-- revPanCake = cakeToBin
revPanCake :: String -> Int
revPanCake xs = minFlips (readBinStr.cakeToBin $ xs) k
                where k = length xs

flipPoint c l = fromMaybe 0 $ findIndex (not.testBit c) [0..l-1]

flipCakeUntil c i l = foldl complementBit c [l-1,l-2..i]

-- testCakes :: STRef s (b, a) -> Int -> ST s b
testCakes s l = do (n,mu) <- readSTRef s
                   let fp = flipPoint mu l
                   let flippedCake = flipCakeUntil mu fp l
                   if popCount mu == l
                     then return n
                     else do writeSTRef s (n+1,flippedCake)--set flipped cake flipCake mu k
                             testCakes s l


-- minFlips :: Int -> Int -> Int
minFlips :: Integer -> Int -> Int
minFlips x l = runST $ do bt <- newSTRef (0,x)
                          testCakes bt l

testCases :: IO ()
testCases = do x<-getLine
               let totalTests = read x::Int
               mapM_ tCase [1..totalTests]
               where tCase x = do nStr <- getLine
                                  let output = show $ revPanCake nStr
                                  putStrLn $ "Case #"++show x++": "++output

main :: IO ()
main = testCases
