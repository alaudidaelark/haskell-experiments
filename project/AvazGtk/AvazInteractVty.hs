module Main where

import qualified Data.Char as Ch
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.HashTable.IO as Hm
import qualified Data.Maybe as Mb
import qualified Data.ByteString.Char8 as DCh8
import qualified Data.Text as T
import qualified Text.Printf as Pr
import qualified Data.Vector as V
import GHC.Float
import System.IO
import NGram
import Control.Monad
import qualified Graphics.Vty.Widgets.All as Vt
import qualified Graphics.Vty.Attributes as At
import qualified Graphics.Vty.LLInput as LLi

textAttr x = if Ch.isUpper $ x!!0
             then colorAttr At.magenta
	     else colorAttr At.blue

colorAttr x= At.with_fore_color At.def_attr x

format (i,x) = [(T.pack$show i++". ",colorAttr At.white)
               ,(T.pack $ (DCh8.unpack $ fst x),textAttr $ DCh8.unpack $ fst x)
               ,(T.pack$" ("++(show $ snd x)++")",colorAttr At.yellow)]

prepareTabl = map format.zip [1..].take 40

applAttr pred = zipWithM_ Vt.setTextWithAttrs pred.prepareTabl

sTotalFreq = F.sum.fmap snd
totalFreq = sum.map snd

percFreq :: [(a,Int)] -> S.Seq (a,Int) -> Int
percFreq dis val = Mb.fromMaybe 0 result
                   where result = if sTotalFreq val /= 0 
		                  then Just $ ((totalFreq dis) * 100) `div` (sTotalFreq val)
				  else Nothing

editHandler pred ng freq wid = do tex <-Vt.getEditText wid
		                  mapM (flip Vt.setText $ T.pack "") pred
				  val <- predictKey ng (DCh8.pack $ T.unpack tex) 3
				  let key n= join.(L.intersperse " ").lastN'  n.words.map Ch.toLower.T.unpack $ tex
				  let dispVal = F.toList $ S.take 40 val
			          applAttr pred dispVal
				  Vt.setText freq $ T.pack $ Pr.printf "%d%% of %d Predictions" (percFreq dispVal val) (S.length val)
		             
padKeyHandler wid key _ = case key of
                          (LLi.KASCII 'q') -> do Vt.shutdownUi
                                                 return True
                          otherwise -> return False

predWidList = replicateM 40 $ Vt.plainText (T.pack "")

column5 = replicate 5 (Vt.column Vt.ColAuto)

ui ng =do avazIn <- Vt.editWidget
	  pred <-predWidList
	  freq <- Vt.plainText (T.pack "Frequency %")
	  tabl <- Vt.newTable column5 Vt.BorderNone
	  centerAvazIn <- Vt.hCentered avazIn
	  hFreq <- Vt.hCentered freq
	  mapM (Vt.addRow tabl) (groupN 5 pred)
	  ctbox <- Vt.vBox centerAvazIn tabl
	  box <- Vt.vBox ctbox hFreq
	  padBox <- Vt.padded box (Vt.padAll 3)
	  fg <- Vt.newFocusGroup
	  Vt.addToFocusGroup fg centerAvazIn
	  Vt.addToFocusGroup fg padBox
	  c <- Vt.newCollection
	  iface1<- Vt.addToCollection c padBox fg
	  avazIn `Vt.onActivate` (editHandler pred ng freq) 
	  padBox `Vt.onKeyPressed` padKeyHandler
	  return c
		    
main :: IO ()
main = do nghtabl <-ngHashMap
          c <- ui nghtabl
	  Vt.runUi c Vt.defaultContext

