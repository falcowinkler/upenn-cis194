module PartySpec where


import Party
import Test.Hspec
import Employee
spec :: Spec
spec = do
  describe "append join lists" $ do
    it "works" $ do
      maxFun testCompany
