module TreeFolds where
import Data.List hiding (insert)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node height left x right)
  | heightLeft <= heightRight = Node heightLeft newLeft x right
  | heightLeft > heightRight = Node heightRight left x newRight
  where
    newLeft = insert a left
    newRight = insert a right
    heightLeft = heightRec newLeft
    heightRight = heightRec newRight
    heightRec Leaf = 0
    heightRec (Node _ l _ r) = 1 + max (heightRec l) (heightRec r)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
