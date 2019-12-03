{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day2Spec (spec) where

import Import
import Day2
import Test.Hspec

spec :: Spec
spec =
  describe "Day2" $ do
    describe "part a" $ do
      it "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)." $
        run "1,0,0,0,99" `shouldBe` [2,0,0,0,99]
      it "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)." $
        run "2,3,0,3,99" `shouldBe` [2,3,0,6,99]
      it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)." $
        run "2,4,4,5,99,0" `shouldBe` [2,4,4,5,99,9801]
      it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
        run "1,1,1,4,99,5,6,0,99" `shouldBe` [30,1,1,4,2,5,6,0,99]
      it "answer" $ do
        input <- readFileUtf8 "inputs/day2.txt"
        solvePartA input `shouldBe` 7594646
    describe "part b" $
      it "answer" $ do
        input <- readFileUtf8 "inputs/day2.txt"
        solvePartB input `shouldBe` 2