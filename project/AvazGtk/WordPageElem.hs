module WordPageElem (wordPageIdMap,wordPageIdTrie,pageIdElemsVec,lookupPage,lookupTrie) where

import qualified Data.ByteString.Char8 as DCh8
import qualified Data.HashTable.IO as Hm
import qualified Data.Trie as Tr
import qualified Data.Vector as V
import qualified Data.Either.Combinators as DCo
import qualified Data.Maybe as DMa
import qualified System.Environment as Env
import qualified System.Directory as Dir
import Safe
import Common

data PE = PE { pagee::Int,elems::[Second] } deriving Show
data WP = WP { word::First,pagew::Int } deriving Show
type HashTable k v = Hm.CuckooHashTable k v

pageElemRgx = DCh8.pack "(\\d*)\\t(.*)"
wordPageRgx = DCh8.pack "(.*)\\t(\\d*)"

pageelem kind n = "pageelems"++show n++"-40"++kind++".txt"
wordpage n = "wordpage"++show n++"-40.txt"

newWPmap :: IO (HashTable DCh8.ByteString Int)
newWPmap = Hm.new

wordPageIdMap = do cwd <- Dir.getCurrentDirectory
                   wp <- mapM (\n -> matchedWP (cwd++"/data/"++(wordpage n)) wordPageRgx) currentGrams
                   wptable <- newWPmap
                   mapM (mapM (\wp -> Hm.insert wptable (word wp) (pagew wp) )) wp
                   return $! wptable

wordPageIdTrie = do cwd <- Dir.getCurrentDirectory
                    wp <- mapM (\n -> matchedWP (cwd++"/data/"++(wordpage n)) wordPageRgx) currentGrams
                    let wptrieone one start = foldr (\a b -> Tr.insert (word a) (pagew a) b) start one
                    let wptrie = foldr wptrieone Tr.empty wp
                    --TODO: define own fold let wptrieone one start = foldl' (\a b -> Tr.insert (word a) (pagew a) b) start one
                    return $! wptrie

pageIdElemsVec = do opts <- Env.getArgs
                    let suff = headDef "" opts
                    cwd <- Dir.getCurrentDirectory
                    pe <- mapM (\n -> matchedPE (cwd++"/data/"++pageelem suff n) pageElemRgx) currentGrams
	            return $! map (V.fromList.(map elems)) pe 

lookupTrie wpTrie peArray x = do let ans = Tr.lookup k wpTrie
                                 case ans of 
                                      Just dat -> return $ (maybe [] id ((peArray !! (pred n)) V.!? dat),dat,succ n)
			              Nothing  -> case n>=1 of 
                                                       True  -> lookupTrie wpTrie peArray (key (pred n) k)
			     		               False -> return ([],0,n)
				 where k = fst $ keyn x
				       n = snd $ keyn x

lookupPage wpMap peArray x = do ans <- Hm.lookup wpMap k
                                case ans of 
                                     Just dat -> return $ (maybe [] id ((peArray !! (pred n)) V.!? dat),dat,succ n)
			             Nothing  -> case n>=1 of 
                                                      True  -> lookupPage wpMap peArray (key (pred n) k)
					              False -> return ([],0,n)
				where k = fst $ keyn x
				      n = snd $ keyn x

readWP inp = WP (inp !! 0) (intByte $ inp !! 1)
readPE inp = PE (intByte $ inp !! 0) (DCh8.split ',' $ inp !! 1)

extractWPReg = (readWP.fourth.DMa.fromJust.DCo.fromRight')
extractPEReg = (readPE.fourth.DMa.fromJust.DCo.fromRight')

matchedWP = matched extractWPReg
matchedPE = matched extractPEReg

