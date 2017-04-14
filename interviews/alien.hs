module Main where

import           Data.List  as L
import           Data.Map   as M
import           Data.Maybe
import           System.IO

data Rule = Rule Char Char Ordering deriving (Eq,Show,Ord)

infer :: (String,String) -> [Rule]
infer (x,y) = ru.dropWhile (uncurry (==)) $ L.zip x y
              where ru [] = []
                    ru ((a,b):_) = [Rule a b GT]

ruleSolve :: [Rule] -> Map Char String
ruleSolve [] =  M.empty
ruleSolve (Rule x y _:xs) = case M.lookup x m of
                                 Just t ->  M.insert x (sort $ se t y)  m
                                 Nothing -> M.insert x [y] m
                            where m = ruleSolve xs
                                  se t k = if k `elem` t
                                             then t
                                             else k:t

testCompleteRuleMap :: Map Char String -> (Bool,String,[(Int,Char)])
testCompleteRuleMap x = (b,res,v)
                        where v = sort [(L.length $ defLookup i x,i)| i<-chs]
                              chs = ['a'..'z']
                              b = L.map fst v == [0..25]
                              res = L.map snd v

defLookup :: Char -> Map Char String -> String
defLookup i x = fromMaybe [] $ M.lookup i x

completeRules :: Map Char String -> Map Char String
completeRules m
  | ans == m = ans
  | otherwise = completeRules ans
                      where ans = M.map op m
                            op v = L.sort.L.nub $ L.union v $ allSubSet v
                            allSubSet = L.foldl' L.union [].L.map subSet
                            subSet ch = defLookup ch m

alienDec :: [String] -> [Rule]
alienDec [] = []
alienDec [_] = []
alienDec ([x,y]) = infer (x,y)
alienDec (x:y:ls) = infer (x,y)++alienDec (y:ls)


readText :: String -> IO [String]
readText fil =do  h<-fHandle fil
                  inp h
                  where inp h = do end <- hIsEOF h
                                   if end then return []
                                   else  do ln <- hGetLine h
                                            remi <- inp h
                                            return (ln:remi)
                        fHandle f = if f == "stdin"
                                      then return stdin
                                      else openFile f ReadMode

answer :: [String] -> String
answer  = out.testCompleteRuleMap.completeRules.ruleSolve.alienDec
          where out (x,y,v) = if x
                              then "Complete "++reverse y
                              else "Incomplete "++mis++reverse y
                                    where mis = showDup v

showDup :: [(Int,Char)] -> String
showDup [] = []
showDup [_] = []
showDup ((x,y):(a,b):xs) = if x==a
                           then [y,',',b,' ']++showDup v
                           else showDup v
                             where v = (a,b):xs

main :: IO ()
main = do allIn <- readText "stdin"
          putStrLn $ answer allIn
