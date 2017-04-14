import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let r = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let g = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let l = read input_line :: Int -- the length of the landing platform.
    hPutStrLn stderr $ show (r,g,l)
    loop (r,g,l,False)

loop :: (Int,Int,Int,Bool) -> IO ()
loop (r,g,l,j) = do
    input_line <- getLine
    let s = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let x = read input_line :: Int -- the position on the road of the motorbike.
   
    hPutStrLn stderr $ "State "++show (s,x)++" Init "++show (r,g,l)
    -- hPutStrLn stderr "Debug messages..."
    let speed =  putStrLn "SPEED">>return j
    let wait = putStrLn "WAIT">>return j
    let slow = putStrLn "SLOW">>return j
    let jump = putStrLn "JUMP">>return True

    let action 
    		| j = slow
    		| r == x+1 = jump
		| s == g+1 = wait
		| s > g+1  = slow
		| s < g+1  = speed
    st <- action  -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    
    loop (r,g,l,st)
