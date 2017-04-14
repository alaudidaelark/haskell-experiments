import Control.Monad

data IOLim =  IOIn (IO String) | IOOut (String -> IO String)

printRetStr :: String -> IO String
printRetStr s = putStrLn s >> return s

a :: [IOLim]
a = [IOOut printRetStr,IOIn getLine,IOOut printRetStr,IOIn getLine]

exec :: IOLim -> IO String -> IO String
exec (IOIn k) t = t >> k
exec (IOOut k) s = s >>= k

execList :: [IOLim] -> IO String
execList = foldl (flip exec) (return "Hello")

main :: IO ()
main = void $ execList a
