module Party where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL employees fun) = (GL (emp:employees) (fun + (empFun emp)))


instance Semigroup GuestList where
  (GL emps1 fun1) <> (GL emps2 fun2) = (GL (emps1 ++ emps2) (fun1 + fun2))

instance Monoid GuestList where
  mempty = GL [] 0
 -- mappend = (<>), is already derived

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL emp1 fun1) gl2@(GL emp2 fun2)
  | fun1 > fun2 = gl1
  | otherwise = gl2

-- regarding Exercise 2
-- trees are actuall foldable


treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f b (Node l forest) = foldr (\tree  otherb -> treeFold f otherb tree) newB forest
  where
    newB = (f l b)
