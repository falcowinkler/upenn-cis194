module GolfSpec where
import Golf
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "skips" $ do
    it "works for sample tests" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True,False] `shouldBe` [[True,False], [False]]
      skips ([] :: [Int]) `shouldBe` []
    it "output matches input length" $ do
      let prop = (\xs -> length xs == length (skips xs)) :: ([Int] -> Bool) in
        quickCheck prop
  describe "local maxima" $ do
    it "should work for sample tests" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []
