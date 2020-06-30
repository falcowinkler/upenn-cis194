{-# LANGUAGE FlexibleInstances #-}
module Vars where
import qualified Data.Map as M
import Data.Maybe
import Calculator (Expr, lit, add, mul)

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var


instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- class Expr a where
--   lit :: Integer -> a
--   add :: a -> a -> a
--   mul :: a -> a -> a


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a m = Just a
  add f g m = do
    fLookup <- f m
    gLookup <- g m
    return (fLookup + gLookup)
  mul f g m = do
    fLookup <- f m
    gLookup <- g m
    return (fLookup * gLookup)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
