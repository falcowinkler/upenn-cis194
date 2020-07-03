module FibSpec where
import Fib
import Test.Hspec

spec :: Spec
spec = do
  describe "fibs" $ do
    it "works the same as trivial solution" $ do
      take 20 fibs1 `shouldBe` take 20 fibs2
