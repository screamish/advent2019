{-# LANGUAGE NoImplicitPrelude #-}
module Day1
  ( fuelForA
  , fuelForB
  , totalFuel
  ) where

import RIO
import RIO.List
import qualified RIO.Text as T
import qualified RIO.Partial as RIO'

fuelForA :: Int -> Int
fuelForA x = max 0 $ (x `div` 3) - 2

fuelForB :: Int -> Int
fuelForB = sum . takeWhile (> 0) . drop 1 . iterate fuelForA

totalFuel :: (Int -> Int) -> Text -> Int
totalFuel fuelCalc =
  sum . fmap (fuelCalc . RIO'.read) . lines . T.unpack