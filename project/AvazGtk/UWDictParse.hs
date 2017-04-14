module UWDict where

--import Prelude hiding (print,readFile)
import System.IO.UTF8 as SUTF8
import Text.ParserCombinators.Parsec as TPa
import Text.ParserCombinators.Parsec.Char as TCh
import Data.Char as DCh
import Data.Either.Combinators as DCo

data UWData = UWData {word :: String, info:: String}

dictFile = do result <- many line
              eof
              return result

line = do result <- container
          newline
          return result

container = do result1 <- between (char '[') (char ']') (many $ noneOf "[]")
               skipMany1 $ noneOf "()"
               result2 <- between (char '(') (char ')') (many $ noneOf "()")
               skipMany1 $ satisfy isPrint
               return $ UWData result1 result2

uwdict = SUTF8.readFile "uwdict.txt"

uwParsed x = fromRight [] (parse dictFile "" x)

main :: IO ()
main =  do content<-uwdict
           mapM_ (SUTF8.print.word) (uwParsed content)
           return ()
