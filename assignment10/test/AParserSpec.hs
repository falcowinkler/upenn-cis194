module AParserSpec where

import Test.Hspec
import Test.QuickCheck
import AParser
import Debug.Trace
import Data.Char

resultOrEmpty :: String -> String
resultOrEmpty s = rest
  where
    (void, rest) = case runParser intOrUppercase $ s ++ "asdf" of
      Nothing -> ((), "")
      Just (a, b) -> (a, b)

alternativeParserProp :: Positive Int -> Bool
alternativeParserProp num = rest == "asdf"
  where rest = resultOrEmpty ((show.getPositive) num)

alternativeParserProp' :: Char -> Bool
alternativeParserProp' c = rest == "asdf"
  where rest = resultOrEmpty [c]

genSafeChar :: Gen Char
genSafeChar = elements ['A'..'A']

spec :: Spec
spec = do
  describe "alternative parser works" $ do
    it "parses numbers correctly " $ property $
      alternativeParserProp
    it "parses chars correctly " $ property $
      forAll genSafeChar $ alternativeParserProp'
