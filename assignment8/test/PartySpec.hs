module PartySpec where


import Party
import Test.Hspec

spec :: Spec
spec = do
  describe "append join lists" $ do
    it "works" $ do
      1 `shouldBe` 1
