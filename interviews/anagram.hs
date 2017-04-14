module Anagram where
import           Data.List

rot :: [a] -> [[a]]
rot [] = []
rot (x:xs) = (x:xs) : rest
             where rest = do (y:ys) <- rot xs
                             return (y:x:ys)

anatest :: Eq a => [a] -> [[a]]
anatest [] = [[]]
anatest xs = concatMap (\(x:y) -> map (x:) $ anatest y) (nub.rot $ xs)

testAnagram :: IO ()
testAnagram = mapM_ putStrLn $ anatest "hello"

main :: IO ()
main = testAnagram
