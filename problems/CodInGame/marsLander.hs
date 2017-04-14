import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of points used to draw the surface of Mars.
    
    surfacePntList <- replicateM n $ do
			input_line <- getLine
			let input = words input_line
			let land_x = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
			let land_y = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
			return (land_x,land_y)
    hPutStrLn stderr $ show surfacePntList
    loop $ safeXRanges surfacePntList


absdist :: Int -> (Int, Int) -> Int
absdist x (x1,x2) = min (abs (x1-x)) (abs (x2-x))

closestPredicate :: (Int,Int) -> (Int,Int) -> Int -> Ordering
closestPredicate (x1,x2) (x3,x4) x 
	| compare x1 x /= compare x2 x = LT
	| compare x3 x /= compare x4 x = GT
	| otherwise = compare (absdist x (x1,x2)) (absdist x (x3,x4))

closestRange :: (Int,Int) -> [(Int,Int,Int)] -> (Int,Int,Int)
closestRange st = minimumBy (\(x1,x2,_) (x3,x4,_) -> closestPredicate (x1,x2) (x3,x4) (fst st)).(filter (\(_,_,y) -> snd st >= y))

safeXRanges :: [(Int,Int)] -> [(Int,Int,Int)]
safeXRanges [] = []
safeXRanges [_] = []
safeXRanges (x:y:lst) = if snd x == snd y then (fst x,fst y,snd x):(safeXRanges lst) else safeXRanges (y:lst)


rotThrustFor :: (Int,Int,Int,Int,Int,Int,Int) -> (Int,Int,Int) -> (Int,Int)
rotThrustFor (x,y,hs,vs,f,r,p) (x1,x2,dy) = (rot,thrust)
	where rot
		| hs == 0 && compare x1 x /= compare x2 x = 0
		| otherwise = (45)*(signum hs)
	      thrust 
	      	| (vs > 0) = 0
		| (vs <= (-40)) = 4
	      	| otherwise = 0
	      

loop :: [(Int,Int,Int)] -> IO ()
loop pointList = do
		input_line <- getLine
		let input = words input_line
		let x = read (input!!0) :: Int
		let y = read (input!!1) :: Int
		let hs = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
		let vs = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
		let f = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
		let r = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
		let p = read (input!!6) :: Int -- the thrust power (0 to 4).
		
		-- hPutStrLn stderr "Debug messages..."
		let param = (x,y,hs,vs,f,r,p)
		let closest = closestRange (x,y) pointList
		hPutStrLn stderr $ show closest
		hPutStrLn stderr $ "Action : "++(show $ rotThrustFor param closest)
		let output (ro,th) = show ro++" "++show th
		-- R P. R is the desired rotation angle. P is the desired thrust power.
		putStrLn $ output $  rotThrustFor param closest
		loop pointList
