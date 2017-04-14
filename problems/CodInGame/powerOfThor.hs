import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position
    loop (lx,ly,tx,ty)

loop :: (Int,Int,Int,Int) -> IO ()
loop (lx,ly,tx,ty) = do
    input_line <- getLine
    let e = read input_line :: Int -- The level of Thor's remaining energy, representing the number of moves he can still make.
    let hori = if lx > tx then 'E' else 'W'
    let vert = if ly > ty then 'S' else 'N'
    let out
   	 	| (lx == tx) = [vert]
    		| (ly == ty) = [hori]
	    	| otherwise  = [vert,hori]
    let currentpos
    		| out == "N" = (lx,ly,tx,ty-1)
		| out == "S" = (lx,ly,tx,ty+1)
		| out == "E" = (lx,ly,tx+1,ty)
		| out == "W" = (lx,ly,tx-1,ty)
		| out == "NE" = (lx,ly,tx+1,ty-1)
		| out == "SE" = (lx,ly,tx+1,ty+1)
		| out == "NW" = (lx,ly,tx-1,ty-1)
		| out == "SW" = (lx,ly,tx-1,ty+1)
    hPutStrLn stderr $show currentpos
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn out
    when (e > 0) $ loop currentpos
