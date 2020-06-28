module TreeFoldsSpec where
import TreeFolds
import Test.Hspec
import Test.QuickCheck

isBalancedTree :: Tree a -> Bool
isBalancedTree Leaf = True
isBalancedTree (Node h left x right) =
  abs (getHeight left - getHeight right) <= 1
  &&
  isBalancedTree left
  &&
  isBalancedTree right
  where
    getHeight Leaf = 0
    getHeight (Node h _ _ _) = h

count Leaf = 0
count (Node _ r _ l) = 1 + count r + count l

prop_foldBalancedInts :: [Int] -> Bool
prop_foldBalancedInts xs = isBalancedTree tree && count tree == length xs
  where
    tree = foldTree xs

prop_foldBalancedStrings :: String -> Bool
prop_foldBalancedStrings  xs = isBalancedTree tree && count tree == length xs
  where
    tree = foldTree xs

spec :: Spec
spec = do
  describe "tree folds" $ do
    it "works for sample tests" $ do
      isBalancedTree $ foldTree [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    it "result is a balanced subtree for some integer lists" $ property $
      prop_foldBalancedInts
    it "result is a balanced subtree for some string lists" $ property $
      prop_foldBalancedInts
