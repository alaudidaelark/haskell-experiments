rev :: String -> String
rev = foldl (flip (:)) []

rev1 :: String -> String
rev1 = f []
       where f x (y:z) = f (y:x) z
             f y _ = y

main :: IO ()
main = do inp <- getLine
          putStrLn $ rev inp
          putStrLn $ rev1 inp
