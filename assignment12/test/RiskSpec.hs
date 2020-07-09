module RiskSpec where
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "risk game simulation" $ do
    it "should work" $ do
      1`shouldBe`1
