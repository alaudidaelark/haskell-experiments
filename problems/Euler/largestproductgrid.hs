module Main where

import Data.ByteString.Char8 as B
import Data.Vector as V
import Data.Maybe as M
import Data.List as L

inputToValues :: ByteString -> Vector (Vector Int)
inputToValues input = V.map V.fromList $ V.fromList (L.map (L.map (fst . M.fromJust . B.readInt)) (L.map B.words $ B.lines input))

valueAtXY:: Vector (Vector Int) -> Int -> Int -> Int
valueAtXY v x y = M.fromJust $ (M.fromJust $ v !? y) !? x

main :: IO ()
main = do {
    input <- B.readFile "largestproductgrid.txt";
	print $ inputToValues input;
	}