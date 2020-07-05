module JoinListSpec where


import JoinList
import Test.Hspec

spec :: Spec
spec = do
  describe "append join lists" $ do
    it "works" $ do
      (Append [2, 1] Empty (Single [2] 'a')) +++ (Append [4, 3] (Single [3] 'a') Empty) `shouldBe`
        (Append [2, 1, 4, 3] (Append [2, 1] Empty (Single [2] 'a')) (Append [4, 3] (Single [3] 'a') Empty))
