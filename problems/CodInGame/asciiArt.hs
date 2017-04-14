import System.IO
import Control.Monad
import Data.Map
import Data.List
import Data.Maybe
import Data.Char


groupCount :: Int -> String -> [String]
groupCount ns ys = g ns ys 
		      where g _ [] = []
		            g n xs = (fst ff):(g n (snd ff)) 
			             where ff = f n [] xs 
		            f _ x [] = (reverse x,[])
		            f 0 x y = (reverse x,y)
			    f n x (a:b) = f (n-1) (a:x) b

splitAscii :: [String] -> (Int,Int) -> [[String]]
splitAscii [] (_,_) = [[]]
splitAscii ys (l,_) = Data.List.transpose $ Data.List.map (groupCount l) ys

asciiMap :: [String] -> (Int,Int) -> String -> Map Char [String]
asciiMap inp (l,h) charList = fromList mList
				where ascList = splitAscii inp (l,h)
				      mList = zip charList ascList

formAscii :: String -> Map Char [String] -> [String]
formAscii inp charMap = Data.List.foldl1' concAscii (Data.List.map (\t -> fromMaybe (charMap ! '?') $ Data.Map.lookup (toUpper t) charMap) inp)
			where concAscii = zipWith (++)

readTest :: IO [String]
readTest = do testInput <- openFile "asciiArt_Test_1_input.txt" ReadMode
	      inpLine <- hGetLine testInput
	      let l = read inpLine :: Int
	      inpLine <- hGetLine testInput
	      let h = read inpLine :: Int
	      t <- hGetLine testInput
	      ascLines <- replicateM h $ do row <- hGetLine testInput;return row
	      let charMapI = (asciiMap ascLines (l,h) (['A'..'Z']++"?"))
   	      mapM_ (\x -> hPutStrLn stderr $ show x)  $ formAscii "HELLo" charMapI
	      return ascLines
      


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    t <- getLine
    
    ascLines <- replicateM h $ do row <- getLine;return row
    
    --mapM_ (\x -> hPutStrLn stderr $ show x) (ascLines)
    
    -- Write answer to stdout
    let charMapI = (asciiMap ascLines (l,h) (['A'..'Z']++"?"))
    mapM_ putStrLn  $ formAscii t charMapI

