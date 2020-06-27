module LogSpec where

import Log
import Test.Hspec
import Test.QuickCheck


instance Arbitrary MessageType where
  arbitrary = do
    code <- arbitrary
    elements [Info, Warning, Error code]

instance Arbitrary LogMessage where
  arbitrary = do
    message <- arbitrary
    messageType <- arbitrary
    ts <- arbitrary
    elements [Unknown message, LogMessage messageType ts message]

instance Arbitrary MessageTree where
  arbitrary = do
    logMessage <- arbitrary
    left <- arbitrary
    right <- arbitrary
    elements [Leaf, Node left logMessage right]

spec :: Spec
spec = do
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


  describe "parse message file" $ do
    it "parses multiple well formatted lines" $ do
      parseFile "I 28 la la la\nE 1 123 explosion\nW 5 warning" `shouldBe`
        [
          LogMessage Info 28 "la la la",
          LogMessage (Error 1) 123 "explosion",
          LogMessage Warning 5 "warning"
        ]
    it "parses also unkknown formats " $ do
      parseFile "I 28 la la la\nE 123 explosion\n5 warning" `shouldBe`
        [
          LogMessage Info 28 "la la la",
          Unknown "E 123 explosion",
          Unknown "5 warning"
        ]

  describe "message tree" $ do
    it "does not insert unchanged" $ do
      let propUnchanged = (\e -> insert (Unknown "") e == e) in
        quickCheck propUnchanged
    it "does insert strictly smaller nodes in left subtree" $ do
      let tree = Node Leaf (LogMessage Info 2 "bla") Leaf in
        insert (LogMessage Warning 1 "blubb") tree `shouldBe`
        Node
          (
            Node
            Leaf (LogMessage Warning 1 "blubb") Leaf
          )
          (LogMessage Info 2 "bla")
          Leaf
    it "does insert greater or equal nodes in right subtree" $ do
      let tree = Node Leaf (LogMessage Info 2 "bla") Leaf in
        insert (LogMessage Warning 2 "blubb") tree `shouldBe`
        Node
          Leaf
          (LogMessage Info 2 "bla")
          (
            Node
            Leaf (LogMessage Warning 2 "blubb") Leaf
          )
