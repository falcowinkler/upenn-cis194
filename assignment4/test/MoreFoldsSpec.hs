module MoreFoldsSpec where
import MoreFolds
import Test.Hspec
import Test.QuickCheck


mapProp :: [Int] -> Bool
mapProp xs = map (+1) xs == map' (+1) xs

spec :: Spec
spec = do
  describe "xor folds" $ do
    it "works for sample tests" $ do
      xor [True, False] `shouldBe` True
    it "works for random tests" $ property $
      \xs -> odd (length (filter id xs)) == xor xs
  describe "map works" $ do
    it "works for random tests" $ property $
      mapProp
