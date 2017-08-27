{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c
    | test "EAIONRTLSU" = Score 1
    | test "DG"         = Score 2
    | test "BCMP"       = Score 3
    | test "FHVWY"      = Score 4
    | test "K"          = Score 5
    | test "JX"         = Score 8
    | test "QZ"         = Score 10
    | otherwise         = mempty
    where u    = toUpper c
          test = elem u

scoreString :: String -> Score
scoreString = mconcat . map score
