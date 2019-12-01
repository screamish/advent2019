{-# LANGUAGE NoImplicitPrelude #-}
module Day1Spec (spec) where

import Import
import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    describe "part a" $ do
      it "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2." $
        fuelForA 12 `shouldBe` 2
      it "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2." $
        fuelForA 14 `shouldBe` 2
      it "For a mass of 1969, the fuel required is 654." $
        fuelForA 1969 `shouldBe` 654
      it "For a mass of 100756, the fuel required is 33583." $
        fuelForA 100756 `shouldBe` 33583
      it "answer" $ do
        input <- readFileUtf8 "inputs/day1.txt"
        totalFuel fuelForA input `shouldBe` 3265923
    describe "part b" $ do
      it "For a mass of 1969, the fuel required is 966." $
        fuelForB 1969 `shouldBe` 966
      it "For a mass of 100756, the fuel required is 50346." $
        fuelForB 100756 `shouldBe` 50346
      it "answer" $ do
        input <- readFileUtf8 "inputs/day1.txt"
        totalFuel fuelForB input `shouldBe` 3265923
    
  describe "Day2" $
    it "TBD" $ 1 `shouldBe` 1
