module Validate where
import Data.Char
digitsFromInt :: Integer -> [Int]
digitsFromInt = (map digitToInt) . show

doubleIfIndexOdd :: Int -> Int -> Int
doubleIfIndexOdd e i
  | (i `mod` 2) == 0 = e
  | otherwise = e * 2

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond xs = zipWith doubleIfIndexOdd (reverse xs) [0..]

narrowGreaterThan9 :: [Int] -> [Int]
narrowGreaterThan9 xs = map (\e -> if e > 9 then e - 9 else e) xs

validateFinalSequence :: [Int] -> Bool
validateFinalSequence xs = (mod (sum xs) 10) == 0

validate :: Integer -> Bool
validate = validateFinalSequence . narrowGreaterThan9 . doubleEverySecond . digitsFromInt
