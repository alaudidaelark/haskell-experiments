module NGram (NGM(..),lastN',groupN,predictKey,ngHashMap,ngStart,ngCategMap,prefixgen) where

import qualified Data.ByteString.Char8 as DCh8
import qualified Data.Char as Ch
import qualified Data.List as L
import qualified Data.Either.Combinators as DCo
import qualified Data.HashTable.IO as Hm
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Maybe as DMa
import Control.Monad
import Common
--import Safe
import qualified System.Directory as Sd
import qualified System.Console.GetOpt as Opt
import qualified Text.Regex.PCRE.Wrap as TWr
import qualified Text.Regex.Base.RegexLike as TRe

data NGM = NGM { count::Count,first::First,second::Second } deriving Show

ngregexinc = DCh8.pack "(\\d*)\\t(.*)\\t(.*)"

appendTo x y = (head x):y

ngIns el m = M.insertWith appendTo (first el) [(second el,count el)] m

ngMapGen ng map = foldr ngIns map ng

ngMap = do ng <- mapM (\x -> matchedNG (ngmFile x) ngregexinc) currentGrams
           return $! foldr ngMapGen M.empty ng

type HashTable k v = Hm.CuckooHashTable k v
newNGmap :: IO (HashTable DCh8.ByteString (S.Seq (DCh8.ByteString,Int)))
newNGmap = Hm.new

appendToNg h k v = do ans <- (Hm.lookup h k)
                      case ans of Just x -> Hm.insert h k (x S.|> v)
                                  Nothing -> Hm.insert h k $ S.singleton v

ngHashMap = do ng <- mapM (\x ->matchedNG (ngmFile x) ngregexinc) currentGrams
	       ngtable <- newNGmap
               mapM (mapM (\ngm -> appendToNg ngtable (first ngm) (second ngm,count ngm))) ng
	       return $! ngtable

respondTo mng x = M.findWithDefault [] x mng

predictKey ngh x n = do ans <- Hm.lookup ngh (key n x)
                        case ans of
                             Just dat -> return dat
                             Nothing -> case n of
                                             1 -> return S.empty
                                             _ -> predictKey ngh (key (pred n) x) (pred n)

readNGM inp = NGM (intByte $ inp !! 0) (inp !! 1) (inp !! 2)

extractNGMReg = (readNGM.fourth.DMa.fromJust.DCo.fromRight')

matchedNG = matched extractNGMReg

