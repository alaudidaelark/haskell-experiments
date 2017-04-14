import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.STRef

--(10 ==) $ length $ nub $ concat $ map show $ map (*1692) [1..10]

testCount :: STRef s (Int,Int,String) -> ST s Int
testCount s = do (x,mu,nums) <- readSTRef s
                 let newnums = nub $ show (x*mu) ++ nums
                 if length newnums == 10
                    then return (x*mu)
                    else do writeSTRef s (x,mu+1,newnums)
                            testCount s

sheepCount :: Int -> Int
sheepCount x = runST $ do bt <- newSTRef (x,1,"")
                          testCount bt

testCases :: IO ()
testCases = do x<-getLine
               let totalTests = read x::Int
               mapM_ tCase [1..totalTests]
               where tCase x = do nStr <- getLine
                                  let n = read nStr::Int
                                  let output = if n == 0
                                                then "INSOMNIA"
                                                else show $ sheepCount n
                                  putStrLn $ "Case #"++show x++": "++output

main :: IO ()
main = testCases
