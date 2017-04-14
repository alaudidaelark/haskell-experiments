import System.IO
import Data.Char
import Numeric
import qualified Data.Text as T

altGroup :: String -> [String]
altGroup [] = []
altGroup (x:xs) = let (f,s) = span (x==) xs in (x:f):altGroup s


chuckMsg :: String -> String
chuckMsg xs
        | head xs == '1' = "0 "++replicate (length xs) '0'++" "
	| head xs == '0' = "00 "++replicate (length xs) '0'++" "
	| otherwise = ""

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    let f x = replicate (7-length x) '0'++x
    let binMsg = concatMap (\x -> f $ showIntAtBase 2 intToDigit (ord x) "") message
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn $ T.unpack.T.strip.T.pack.concatMap chuckMsg.altGroup $ binMsg
