module Main where

import qualified Data.Vector as V
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as M
import qualified Data.List as Li

--readInput :: String -> V.Vector (V.Vector Int)
--readInput x = V.map (V.map read) (V.map words $ V.lines x)

readBC :: BC.ByteString -> [[Int]]
readBC x = map (\x-> map ((L.view L._1).M.fromMaybe (0,BC.pack "").BC.readInt) x) arrayelems
          where arrayelems = Li.map BC.words $ BC.lines x

toVectFromListofInts :: [[Int]] -> V.Vector (V.Vector Int)
toVectFromListofInts x = V.fromList (Li.map V.fromList x)

indexy :: (V.Vector (V.Vector Int)) -> Int -> Int -> Int
indexy inda x y = M.fromMaybe 0 $ (M.fromMaybe V.empty (inda V.!? y)) V.!? x

take4by4box :: (V.Vector (V.Vector Int)) -> Int -> Int -> [Int]
take4by4box indata h v = [indexy indata x y| x<-[h..(h+3)],y<-[v..(v+3)]]

groupN _ [] = []
groupN n xs = first:(groupN n second)
              where first = take n xs
	            second = drop n xs

prodhorimax flatboxdata = Li.maximum $ Li.map Li.product $ groupN 4 flatboxdata

prodvertimax flatboxdata = Li.maximum $ Li.map Li.product $ Li.transpose $ groupN 4 flatboxdata

prodslashmax flatboxdata = Li.product $ Li.map (\x ->flatboxdata !! x) [0,5..15]

prodbackslashmax flatboxdata = Li.product $ Li.map (\x -> flatboxdata !! x) [3,6..12]

boxmax flatboxdata = Li.maximum $ Li.map (\x -> x flatboxdata) [prodhorimax,prodvertimax,prodslashmax,prodbackslashmax]

readData :: FilePath -> IO (V.Vector (V.Vector Int))
readData x = do indata <- BC.readFile x
                return $ toVectFromListofInts.readBC $ indata

main :: IO ()
main = do doubleArray <- readData "prob11.in"
          let outputdata = Li.maximum $ Li.map boxmax [take4by4box doubleArray h v| h<-[0..16],v<-[0..16]]
          print outputdata

