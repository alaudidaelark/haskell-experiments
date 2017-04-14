module Main where

numWordLength :: String -> Int
numWordLength "0" = 0
numWordLength "1" = 3
numWordLength "2" = 3
numWordLength "3" = 5
numWordLength "4" = 4
numWordLength "5" = 4
numWordLength "6" = 3
numWordLength "7" = 5
numWordLength "8" = 5
numWordLength "9" = 4
numWordLength "10" = 3
numWordLength "11" = 6
numWordLength "12" = 6
numWordLength "13" = 8
numWordLength "14" = 8
numWordLength "15" = 7
numWordLength "16" = 7
numWordLength "17" = 9
numWordLength "18" = 8
numWordLength "19" = 8
numWordLength "20" = 6
numWordLength ('2':x:[]) = 6+numWordLength [x]
numWordLength ('3':x:[]) = 6+numWordLength [x]
numWordLength ('4':x:[]) = 6+numWordLength [x]
numWordLength ('5':x:[]) = 5+numWordLength [x]
numWordLength ('6':x:[]) = 5+numWordLength [x]
numWordLength ('7':x:[]) = 7+numWordLength [x]
numWordLength ('8':x:[]) = 6+numWordLength [x]
numWordLength ('9':x:[]) = 6+numWordLength [x]
numWordLength (h:'0':'0':[]) = numWordLength [h]+7
numWordLength (h:'0':x:[]) = numWordLength [h]+10+numWordLength [x]
numWordLength (h:x:y:[]) = numWordLength [h]+10+numWordLength [x,y]
numWordLength "1000" = 11

main :: IO ()
main = print $ sum (map (numWordLength.show) [1..1000]) 
