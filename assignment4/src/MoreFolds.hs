module MoreFolds where
import Data.List (foldl')
import qualified Data.Bits as Bits

xor :: [Bool] -> Bool
xor = foldl' (\c e -> if e then Bits.xor c e else c) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldl' (\c e-> c ++ [f e]) []
