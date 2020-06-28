module WholemealSpec where
import Wholemeal
import Test.Hspec
import Test.QuickCheck
import Debug.Trace



propFunc2 :: Positive Integer -> Bool
propFunc2 x = fun2' (getPositive x) == fun2 (getPositive x)

spec :: Spec
spec = do
  describe "wholemeal programming" $ do
    it "fun1 and fun1' are the same" $ property $
      \x -> fun1' x == fun1 x
    it "fun2 and fun2' are the same" $ property $
      propFunc2
