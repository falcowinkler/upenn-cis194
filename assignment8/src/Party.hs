module Party where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL employees fun) = (GL (emp:employees) (fun + (empFun emp)))


instance Semigroup GuestList where
  (<>) = moreFun

instance Monoid GuestList where
  mempty = GL [] 0
 -- mappend = (<>), is already derived

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL emp1 fun1) gl2@(GL emp2 fun2)
  | fun1 > fun2 = gl1
  | otherwise = gl2

-- regarding Exercise 2
-- trees are actuall foldable

-- folding over a tree of arbitrary children count requires
-- the subtree [b] as agument in f
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f b (Node l forest) = f l (map (treeFold f b) forest)


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob subtrees = ((glCons bob bestSubresult), bestSubresult)
  where
    (bestSubtreeWithBoss, bestSubtreeWithoutBoss) = mconcat subtrees
    bestSubresult = moreFun bestSubtreeWithBoss bestSubtreeWithoutBoss

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withCEO withoutCEO
  where
    (withCEO, withoutCEO) = treeFold nextLevel (mempty, mempty) tree

-- bit unclear what is expected in the task
-- people right under the boss have zero fun,
-- but `glsCons` should just add the fun score when appending?
party :: IO ()
party = do
  company <- readFile "./company.txt"
  putStrLn (show $ maxFun $ read company)
