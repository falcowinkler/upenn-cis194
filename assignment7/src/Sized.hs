{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Semigroup
import JoinList
import Buffer
import Scrabble

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Semigroup Size where
  (Size a) <> (Size b) = Size $ a + b

instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

decideIndexJ :: (Sized b, Monoid b) => Int -> Int -> JoinList b a -> Maybe a
decideIndexJ leftSize i (Append m left right)
  | leftSize <= i = indexJ (i - leftSize) right
  | otherwise = indexJ i left

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i branch@(Append m left right)
  | i < 0 || i >= getSize (size m) = Nothing
  | otherwise =
      let leftSize = getSize $ size $ tag left in
      let rightSize = tag right in
        decideIndexJ leftSize i branch


decideDropJ :: (Sized b, Monoid b) => Int -> Int -> JoinList b a -> JoinList b a
decideDropJ leftSize n (Append m left right)
  | n == leftSize = right
  | n > leftSize = dropJ (n - leftSize) right
  | n < leftSize = (Append m (dropJ n left) right)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n leaf@(Single m a)
  | n <= 0 = leaf
  | otherwise = Empty
dropJ n branch@(Append m left right)
  | n < 0 = branch
  | n >= getSize (size m) = Empty
  | otherwise =
      let leftSize = getSize $ size $ tag left in
      let rightSize = tag right in
        decideDropJ leftSize n branch


decideTakeJ :: (Sized b, Monoid b) => Int -> Int -> JoinList b a -> JoinList b a
decideTakeJ leftSize n (Append m left right)
  | n == leftSize = left
  | n > leftSize = (Append m left (takeJ (n-leftSize) right))
  | n < leftSize = takeJ n left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n leaf@(Single m a)
  | n <= 0 = Empty
  | otherwise = leaf
takeJ n branch@(Append m left right)
  | n <= 0 = Empty
  | n >= getSize (size m) = branch
  | otherwise =
      let leftSize = getSize $ size $ tag left in
      let rightSize = tag right in
        decideTakeJ leftSize n branch



