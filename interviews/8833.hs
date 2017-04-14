import           Control.Monad (replicateM)
import           Data.List
import           Data.Ratio

rot :: [a] -> [[a]]
rot [] = []
rot (x:xs) = (x:xs) : rest
             where rest = do (y:ys) <- rot xs
                             return (y:x:ys)

anagram :: Eq a => [a] -> [[a]]
anagram [] = [[]]
anagram xs = concatMap (\(x:y) -> map (x:) $ anagram y) (nub.rot $ xs)

newtype Operation =  Operation (Rational->Rational->Rational)

showFunc :: Operation -> String
showFunc (Operation x) = case 6 `x` 2 of 8 -> "+"
                                         4 -> "-"
                                         12 -> "*"
                                         3 -> "/"
                                         _ -> "?"

instance Show Operation where
  show  = showFunc

allExpr ::[Rational] -> [Operation] -> [([Rational],[Operation])]
allExpr nl ol = do n <- anagram nl
                   s <- replicateM (pred $ length nl) ol
                   return (n,s)

allExpAns :: [Rational] -> [Operation] -> [([String],Rational)]
allExpAns nl ol = map (\(x,y) -> (twine (map (show.(floor :: Double -> Integer).fromRational) x) (map show y),opFold x y)) $ allExpr nl ol

twine :: [String] -> [String] -> [String]
twine x [] = x
twine [] y = y
twine (x:xs) (y:ys) = x:y:twine xs ys

opFold :: [Rational] -> [Operation]-> Rational
opFold [] _ = 1%1
opFold (x:_) [] = x
opFold (x:xs) (op@(Operation o):os) = if show op == "/" && y==0 then 0 else x `o` y
                                      where y = opFold xs os

test8833 :: Rational -> [Rational] -> [Operation] -> IO ()
test8833 n nl ol = mapM_ (putStrLn.concat.fst) $ filter (\x -> snd x == n) $ allExpAns nl ol

opList :: [Operation]
opList = map Operation [(+),(-),(*),(/)]

numList :: [Rational]
numList = [8,8,3,3]

main :: IO ()
main = test8833 24 numList opList
