module JoinList where
import Scrabble
import Buffer

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (mappend m1 m2) l1 l2
  where
    m1 = tag l1
    m2 = tag l2

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

