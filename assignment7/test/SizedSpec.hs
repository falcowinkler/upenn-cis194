{-# LANGUAGE FlexibleInstances #-}
module SizedSpec where

import Sized
import JoinList
import Test.Hspec
import Test.QuickCheck hiding (getSize)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


--data JoinList m a = Empty
--  | Single m a
--  | Append m (JoinList m a) (JoinList m a)
--  deriving (Eq, Show)

instance Arbitrary Size where
  arbitrary = do
    int <- arbitrary
    return $ Size int

instance Arbitrary (JoinList Size String) where
  arbitrary = do
    left <- arbitrary
    right <- arbitrary
    let leftSize = tag left
    let rightSize = tag right
    leafData <- arbitrary
    elements [Empty, (Single (Size 1) leafData),
              Append (Size $ (getSize rightSize) + (getSize (leftSize))) left right]

listIndexingProperty :: Int -> JoinList Size String -> Bool
listIndexingProperty = \i jl -> (indexJ i jl) == (jlToList jl !!? i)

dropNProperty :: Int -> JoinList Size String -> Bool
dropNProperty = \n jl -> jlToList (dropJ n jl) == drop n (jlToList jl)

takeNProperty :: Int -> JoinList Size String -> Bool
takeNProperty = \n jl -> jlToList (takeJ n jl) == take n (jlToList jl)

spec :: Spec
spec = do
  describe "indexJ" $ do
    it "works with random trees" $ property $
      listIndexingProperty
  describe "dropJ" $ do
    it "works with random trees" $ property $
      takeNProperty
