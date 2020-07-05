module ScrabbleSpec where

import Scrabble
import Test.Hspec
import JoinList

spec :: Spec
spec = do
  describe "scoring functions" $ do
    it "scores characters correctly" $ do
      scoreLine "yay " +++ scoreLine "haskell!" `shouldBe`
        (Append (Score 23)
         (Single (Score 9) "yay ")
         (Single (Score 14) "haskell!"))
