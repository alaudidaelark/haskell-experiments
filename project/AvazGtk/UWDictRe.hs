module UWDictRe (matchedUW,UWD(..))where

import qualified Data.ByteString.Char8 as DCh8
import qualified Data.Either.Combinators as DCo
import qualified Data.Maybe as DMa
import qualified Data.Text as DTe
import qualified Data.Text.Encoding as DEn
import qualified Data.Text.IO as DIO
import qualified Text.Regex.PCRE.ByteString as TBy
import qualified Text.Regex.PCRE.Wrap as TWr
import qualified Text.Regex.Base.RegexLike as TRe

type Cid = Int

type Parent = Int

type Phrase = String

data UWD = UWD { phrase::Phrase,cid::Cid,parent::Parent } deriving Show

fourth (_, _, _,x) = x

intByte = (fst.DMa.fromJust.DCh8.readInt)

readUWD line = UWD (DCh8.unpack $ line !! 0) (intByte $ line !! 6) (intByte $ line !! 7)

uwregex = DCh8.pack "\\[(.*)\\].*\\(LEX=(.*), ABT=.*POS=(.*), LST=.*NUM=(.*), PAR=.*IMG=(.*), AUDIO=(.*),.*CID=(.*), PARENT=(.*), AS"

testCase = DCh8.pack "[bad]{0} \"4\" (LEX=T, ABT=, ANI=, SEM=, POS=adjective, LST=, FRA=, MOR=, NUM=4, PAR=, IMG=descriptives/feelings/bad.png, AUDIO=none, CID=4, PARENT=1, AS=, SP=, EN=Y) <>;"

uwcompiled = TBy.compile TBy.compBlank TBy.execBlank uwregex

extractReg = (readUWD.fourth.DMa.fromJust.DCo.fromRight')

runMap inp compl = mapM (TBy.regexec (DCo.fromRight' compl)) (DCh8.lines inp)

matchedUW = do uwdict <- DCh8.readFile "uwdict.txt"
               compiled <-uwcompiled
               matched <- runMap uwdict compiled
               return $ map extractReg matched

{-main :: IO ()
main = do matched <- matchedUW
          print $ matched !! 0
          return ()
          out <- TBy.regexec (DCo.fromRight' compiled) testCase
          print $ extractReg out
          matchedUW <- runMap uwdict compiled
          mapM_ (print.length.extractReg) matchedUW
-}
