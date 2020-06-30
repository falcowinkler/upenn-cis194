{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calculator where
import Parser hiding (Expr)
import qualified StackVM as VM

data ExprT = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right


evalStr :: String -> Maybe Integer
evalStr xs = case parsed of
  Nothing -> Nothing
  Just e -> Just $ eval e
  where
    parsed = parseExp Lit Add Mul xs

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax { getIntMinMax :: Integer } deriving (Eq, Show)
newtype Mod7 = Mod7 { getIntMod7 :: Integer } deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = \a b -> MinMax $ max (getIntMinMax a) (getIntMinMax b)
  mul = \a b -> MinMax $ min (getIntMinMax a) (getIntMinMax b) -- can i write it shorter?

instance Expr Mod7 where
  lit = Mod7
  add = \a b -> Mod7 $ mod ((getIntMod7 a)  + (getIntMod7 b)) 7
  mul = \a b -> Mod7 $ mod ((getIntMod7 a) * (getIntMod7 b)) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


instance Expr VM.Program where
  lit = pure . VM.PushI
  add p1 p2 = p1 ++ p2 ++ [VM.Add]
  mul p1 p2 = p1 ++ p2 ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

testCompile = testExp :: Maybe VM.Program

