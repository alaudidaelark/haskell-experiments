import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    if n > 0 then do 
    		temps <- getLine
		let rawTemps = map read $ words temps
    		let tempints = zip (map abs rawTemps::[Int]) rawTemps
    		hPutStrLn stderr $ show tempints
    		let result = snd $ minimumBy minPred tempints
			where  minPred (x1,y1) (x2,y2)
				| x1 == x2 = compare y2 y1
				| otherwise =  compare x1 x2
	    -- the N temperatures expressed as integers ranging from -273 to 5526
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    		putStrLn $ show result
    else putStrLn "0"
