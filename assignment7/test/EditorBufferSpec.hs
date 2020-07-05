{-# LANGUAGE FlexibleInstances #-}
module EditorBufferSpec where


import JoinList
import Test.Hspec
import Test.QuickCheck hiding (getSize)
import Sized
import Scrabble
import EditorBuffer
import Buffer


genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ [' '] ++ ['A'..'Z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

instance Arbitrary Size where
  arbitrary = do
    int <- arbitrary
    return $ Size int

instance Arbitrary Score where
  arbitrary = do
    int <- arbitrary
    return $ Score int

instance Arbitrary (JoinList (Score, Size) String) where
  arbitrary = do
    left <- arbitrary
    right <- arbitrary
    let (leftScore, leftSize) = tag left
    let (rightScore, rightSize) = tag right
    leafData <- genSafeString
    elements [Empty, (Single (scoreString leafData, Size 1) leafData),
              Append (Score (getScore leftScore + getScore rightScore),
                      Size (getSize leftSize + getSize rightSize)) left right]

fromToStringProperty :: (JoinList (Score, Size) String) -> Bool
fromToStringProperty l = toString t == s
  where
    s = toString l
    t :: (JoinList (Score, Size) String)
    t = fromString s

toFromStringProperty :: String -> Bool
toFromStringProperty s = (toString joinList) == s
  where
    joinList :: (JoinList (Score, Size) String)
    joinList = (fromString s)

spec :: Spec
spec = do
  describe "buffer instance for JoinList (Score, Size) String works" $ do
    it "to string works with random tests" $ property $
       fromToStringProperty
    it "from string works with random tests" $ property $
      forAll genSafeString $ toFromStringProperty
