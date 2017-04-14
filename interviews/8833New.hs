{-# LANGUAGE GADTs #-}

module AnagEval where

import           Control.Applicative
import           Data.List
import           Data.Ratio

rot :: [a] -> [[a]]
rot [] = []
rot (x:xs) = (x:xs) : rest
             where rest = do (y:ys) <- rot xs
                             return (y:x:ys)

anagram :: Eq a => [a] -> [[a]]
anagram [] = [[]]
anagram xs = nub $ concatMap (\(x:y) -> map (x:) $ anagram y) (nub.rot $ xs)

anag8338 :: (Fractional a,Eq a) => [[AnagExp a]]
anag8338 = map (map AVal) (anagram [8,8,3,3])

applyEveryAnagToEveryExp ::[(String, Maybe Rational)]
applyEveryAnagToEveryExp = do [a,b,c,d] <- anag8338
                              e <- allExprList
                              return (showAnagExp (e a b c d),
                                      evalAnagExp (e a b c d))

data AnagExp t where
  ASum :: Fractional t => AnagExp t->AnagExp t->AnagExp t
  AMin :: Fractional t => AnagExp t->AnagExp t->AnagExp t
  APro :: Fractional t => AnagExp t->AnagExp t->AnagExp t
  ADiv :: Fractional t => AnagExp t->AnagExp t->AnagExp t
  AVal :: Fractional t => Rational -> AnagExp t

expList :: Fractional a => [AnagExp a -> AnagExp a -> AnagExp a]
expList = [ASum,AMin,APro,ADiv]

allExprList :: Fractional a =>  [AnagExp a -> AnagExp a ->
                                AnagExp a -> AnagExp a -> AnagExp a]
allExprList = concat $ do t <- expList
                          s <- expList
                          r <- expList
                          return [\a b c d -> t (s a b) (r c d),
                                  \a b c d -> t a (s b (r c d))]
                                  -- \a b c d -> t a (s (r b c) d),
                                  -- \a b c d -> t (s (r a b) c) d]

showAnagExp :: AnagExp Rational -> String
showAnagExp (ASum k v) = "("++showAnagExp k++"+"++showAnagExp v++")"
showAnagExp (AMin k v) = "("++showAnagExp k++"-"++showAnagExp v++")"
showAnagExp (APro k v) = "("++showAnagExp k++"*"++showAnagExp v++")"
showAnagExp (ADiv k v) = "("++showAnagExp k++"/"++showAnagExp v++")"
showAnagExp (AVal k)   = show.numerator $ k

evalAnagExp :: (Fractional a,Eq a) => AnagExp a -> Maybe Rational
evalAnagExp (ASum k v) = (+) <$> evalAnagExp k <*>  evalAnagExp v
evalAnagExp (AMin k v) = (-) <$> evalAnagExp k <*>  evalAnagExp v
evalAnagExp (APro k v) = (*) <$> evalAnagExp k <*>  evalAnagExp v
evalAnagExp (ADiv k (ADiv n m)) = evalAnagExp (ADiv (APro k m) n)
evalAnagExp (ADiv k v)
  | evalAnagExp v == pure 0 = empty
  | otherwise = (/) <$> evalAnagExp k <*>  evalAnagExp v
evalAnagExp (AVal k) = pure k

filteredResult ::  [(String, Maybe Rational)]
filteredResult = filter ((pure 24 ==) . snd) applyEveryAnagToEveryExp

main :: IO ()
main = mapM_ (putStrLn.init.tail.fst) filteredResult
