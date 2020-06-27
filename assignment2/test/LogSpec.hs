module LogSpec where

import Log
import Test.Hspec

spec :: Spec
spec =
  describe "parseMessage" $ do
    it "parses well formed messages" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "E 1 744 this is an error" `shouldBe`
        LogMessage (Error 1) 744 "this is an error"
      parseMessage "W 5 warning" `shouldBe` LogMessage Warning 5 "warning"
      parseMessage "I 1" `shouldBe` LogMessage Info 1 ""
    it "parses messages of unknown format" $ do
      parseMessage "I abc" `shouldBe` Unknown "I abc"
      parseMessage "E 755 error" `shouldBe` Unknown "E 755 error"
