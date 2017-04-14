module Main where 

import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- The code below will read all the game information for you.
    -- On each game turn, information will be available on the standard input, you will be sent:
    -- -> the total number of visible enemies
    -- -> for each enemy, its name and distance from you
    -- The system will wait for you to write an enemy name on the standard output.
    -- Once you have designated a target:
    -- -> the cannon will shoot
    -- -> the enemies will move
    -- -> new info will be available for you to read on the standard input.
    
    loop

minDistEnemy :: [(String,Int)] -> String
minDistEnemy [] = ""
minDistEnemy [(x,_)] = x
minDistEnemy (x:y:lst)
	| snd x < snd y =  minDistEnemy (x:lst)
	| otherwise = minDistEnemy (y:lst)

loop :: IO ()
loop = do
    input_line <- getLine
    let count = read input_line :: Int -- The number of current enemy ships within range
    
    enemyList <- forM [1..count] $ const $ do
        		input_enemy <- getLine
        		let input = words input_enemy
        		let enemy = head input -- The name of this enemy
        		--putStrLn enemy
        		let dist = read (input!!1) :: Int -- The distance to your cannon of this enemy
        		return (enemy,dist)
    putStrLn $ minDistEnemy enemyList 
    -- hPutStrLn stderr "Debug messages..."
    
    -- The name of the most threatening enemy (HotDroid is just one example)
    --putStrLn enemy
    
    loop
