module XI where

import Data.ByteString.Char8 as B
import Data.Vector as V
import Data.Maybe as M
import Data.List as L

inputToValues :: ByteString -> Vector (Vector Int)
inputToValues input = V.map V.fromList $ V.fromList (L.map (L.map (fst . M.fromJust . B.readInt)) (L.map B.words $ B.lines input))

valueAtXY:: Vector (Vector Int) -> Int -> Int -> Int
valueAtXY v x y = M.fromJust $ (M.fromJust $ v !? y) !? x

takeHoriz :: [Int] -> [Int]
takeHoriz xs = case xs of w:x:y:z:[] -> (w*x*y*z):[]
                          l:m:n:o:p  -> (l*m*n*o):(takeHoriz (m:n:o:p))

--takeSlash :: [[Int]] -> [Int]
--takeSlash xs = case xs of w:x:y:z:[] -> (takeHoriSlash w x y z):[]
--                          l:m:n:o:p  -> (takeHoriSlash l m n o):(takeSlash (m:n:o:p))

--takeHoriSlash :: [Int] -> [Int] -> [Int] -> [Int] -> Int
--takeHoriSlash w x y z = case w of l:m:n:o:[] -> 

inputToValue :: String -> [[Int]]
inputToValue input = map (map read) $ map words $ lines input

maxHoriz :: Vector (Vector Int)
maxHoriz xs = xs

maxVerti :: Vector (Vector Int)
maxVerti xs = (V.transpose xs)

--maxSlash :: [[Int]] -> Int
--maxSlash xs = 

problem :: ByteString -> Int
problem input =  max (maxHoriz $ inputToValue input) (maxVerti $ inputToValue input)

main :: IO ()
main = do
         input<-B.readFile "XI.in"
         print $ problem input
