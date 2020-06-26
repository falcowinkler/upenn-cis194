module HanoiSpec where

import Hanoi
import Test.Hspec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "solves simple" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
    it "solves 4 peg hanoi with 15 disks" $ do
      length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129
