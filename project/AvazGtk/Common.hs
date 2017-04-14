module Common where

import qualified Data.ByteString.Char8 as DCh8
import Control.Monad
import qualified Data.Char as Ch
import qualified Data.List as L
import qualified Text.Regex.PCRE.ByteString as TBy
import qualified Data.Either.Combinators as DCo
import qualified Data.Maybe as DMa
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified System.Directory as Dir

type First = DCh8.ByteString
type Second = DCh8.ByteString
type Count = Int

currentGrams = [2,3,4]

prefix x = "/Users/malar/Documents/Devel/PicturePrediction/src/"++x
prefixgen x = (prefix "temp/")++x
prefixint x = (prefix "inputs/")++x

ngmCategFile = "stemmeduwcateg.txt"
ngmStartFile = "manual18sorted.txt"
--ngm2newFile = prefixgen "pagefile2.txt"
--ngm3newFile = prefixgen "pagefile3.txt"
--ngm4newFile = prefixgen" pagefile4.txt"
ngmFile n = prefixgen "ufs"++show n++"gram.txt"
--ngm2File = prefixint "ufs2gram.txt"
--ngm3File = prefixint "ufs3gram.txt"
--ngm4File = prefixint "ufs4gram.txt"
ngStartRgx = DCh8.pack "(.*)"

insertTo x y = Set.insert ((Set.toList x)!!0) y

ngStart :: IO [String]
ngStart = do cwd <- Dir.getCurrentDirectory
             ngStartData <- DCh8.readFile (cwd++"/data/"++ngmStartFile)
             compl <- rgxcompiled ngStartRgx
             matchedStart <-runMap ngStartData compl
	     return $! map (DCh8.unpack.(flip (!!) $ 0).fourth.DMa.fromJust.DCo.fromRight') matchedStart

ngCategRgx = DCh8.pack "(.*)\\t(.*)"

ngCatIns el m = M.insertWith insertTo (fst el) (Set.singleton $ snd el) m

ngCategMap = do cwd <- Dir.getCurrentDirectory
                ngCategData <- DCh8.readFile (cwd++"/data/"++ngmCategFile)
                compl <- rgxcompiled ngCategRgx
		matchedCateg <- runMap ngCategData compl
		let categItems = map ((\x->(DCh8.unpack $ x!!1,DCh8.unpack $ x!!0)).fourth.DMa.fromJust.DCo.fromRight') matchedCateg
		return $! foldr ngCatIns M.empty categItems

fourth (_, _, _,x) = x

key n= DCh8.pack.join.(L.intersperse " ").lastN' n.words.map Ch.toLower.DCh8.unpack

lastN' n xs = L.foldl' (const .drop 1) xs (drop n xs)

groupN _ [] = []
groupN n xs = first:(groupN n second)
              where first = take n xs
	            second = drop n xs

matched extf fil reg = do ngram <- DCh8.readFile fil
                          compiled <- rgxcompiled reg
                          matched <- runMap ngram compiled
			  return $ map extf matched

rgxcompiled str= TBy.compile TBy.compBlank TBy.execBlank str

runMap inp compl = mapM (TBy.regexec (DCo.fromRight' compl)) (DCh8.lines inp)

intByte = fst.DMa.fromJust.DCh8.readInt

realkey = lastN' (pred.last $ currentGrams).words.map Ch.toLower.DCh8.unpack

keyn k = (DCh8.pack.join.(L.intersperse " ").realkey $ k,length(realkey k))
