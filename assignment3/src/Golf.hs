module Golf where
import Data.List
import Data.Char


--------
every :: [a] -> Int -> [a]
every [] _ = []
every xs n = (take 1 $ drop (n - 1) xs) ++ (every (drop n xs) n)

skips :: [a] -> [[a]]
skips xs = map (every xs) [1..(length xs)]
--------
localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat $ zipWith3 (\a b c -> if a < b && c < b then [b] else []) xs (tail xs) (drop 2 xs)
--------
line :: Int -> [(Int, Int)] -> Int -> String
line maxCount counts no
  | no == 0 = map (intToDigit . fst) counts
  | no == 1 = take (length counts) (repeat '=')
  | otherwise = map (\(digit, count) -> if (no - 2) < count then '*' else ' ') counts

counter :: Ord a => [a] -> [(a, Int)]
counter xs = map (\l -> ( head l, length l)) (group (sort xs))

-- Integers not present are not shown in histogram
histogram :: [Int] -> String
histogram xs = intercalate "\n" lines
  where
    lines = map (line maxCount counts) (reverse [0..(maxCount + 2)])
    maxCount = maximum (map snd counts)
    counts = counter xs
