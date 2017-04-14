{-# LANGUAGE ForeignFunctionInterface #-}

module HsUlapi

(newFreqStemmer
,mostFrequentStem
,DataDir
,FirstLang
,SecondLang
)

where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal

data FreqStem = FreqStem
type DataDir = String
type FirstLang = String
type SecondLang = String

foreign import ccall "frequentstem.h FreqStem_new" c_freqStemmer :: CString -> CString ->CString -> IO (Ptr FreqStem)
foreign import ccall "frequentstem.h mostfreq" c_mostfrequentstem :: Ptr FreqStem -> CString -> CString -> IO CInt

newFreqStemmer :: DataDir -> FirstLang -> SecondLang -> IO (Ptr FreqStem)
newFreqStemmer datadir flang slang = do dataD <- newCString datadir
                                        fL <- newCString flang
					sL <- newCString slang
					c_freqStemmer dataD fL sL

mostFrequentStem :: Ptr FreqStem -> String -> IO (Bool,String)
mostFrequentStem stemptr str = do inpdata <- newCString str
				  outdata <- newCString ""
				  status <- c_mostfrequentstem stemptr inpdata outdata
				  let outstate = Foreign.Marshal.toBool status
				  output <- Foreign.C.peekCString outdata
				  return (outstate,output)

