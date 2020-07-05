module Scrabble where
import qualified Data.Map.Strict as M
import Data.List
import Data.Char

scoreMap = M.fromList$ [
  ('E', 1), ('A', 1), ('I', 1), ('O', 1), ('N', 1), ('R', 1), ('T', 1), ('L', 1), ('S', 1), ('U', 1),
  ('D', 2), ('G', 2),
  ('B', 3), ('C', 3), ('M', 3), ('P', 3),
  ('F', 4), ('H', 4), ('V', 4), ('W', 4), ('Y', 4),
  ('K', 5),
  ('J', 8), ('X', 8),
  ('Q', 10), ('Z', 10)]

score :: Char -> Score
score c = case M.lookup c scoreMap of
  Nothing -> Score 0
  Just i -> Score i


newtype Score = Score { getScore :: Int } deriving (Show, Eq)

instance Semigroup Score where
  (Score a)  <> (Score b) = Score $ a + b

instance Monoid Score where
  mempty = (Score 0)
  mappend (Score a) (Score b) = Score $ a + b

scoreString :: String -> Score
scoreString = foldl' mappend mempty . map score . map toUpper
