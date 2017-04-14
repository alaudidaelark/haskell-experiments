module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Map

data CubeColor = Black | Blue | Red | Green | Yellow | Orange
                 deriving (Eq,Ord,Enum)
data CubeSide = Front | Back | Lef | Righ | Top | Bottom
                deriving (Eq,Ord,Enum)

sidePairs :: [(CubeSide, CubeSide)]
sidePairs = [(Lef,Top),(Top,Righ),(Righ,Bottom),(Bottom,Lef)]

chefPairWise :: Map CubeSide CubeColor -> String
chefPairWise colSideMap = if topbottom then "YES" else "NO"
                          where topbottom = any
                                            (\(x,_) ->
                                            any (sameColor x) [Front,Back])
                                            fourSides
                                fourSides = Prelude.filter (uncurry sameColor)
                                            sidePairs
                                sameColor x y = colSideMap ! x == colSideMap ! y

instance Read CubeColor where
  readsPrec _ "black" = [(Black,"")]
  readsPrec _ "blue"  = [(Blue,"")]
  readsPrec _ "red"   = [(Red,"")]
  readsPrec _ "green" = [(Green,"")]
  readsPrec _ "yellow"= [(Yellow,"")]
  readsPrec _ "orange"= [(Orange,"")]
  readsPrec _ _ = []

runTests :: IO ()
runTests = do colorList <- Prelude.map read.words <$> getLine
              let sideColorMap = fromList $
                                 zip (enumFrom Front) (colorList::[CubeColor])
              putStrLn $ chefPairWise sideColorMap

main :: IO ()
main = do noOfTests <- read <$> getLine
          replicateM_ noOfTests runTests
