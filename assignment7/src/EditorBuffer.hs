{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module EditorBuffer where
import Buffer
import JoinList
import Scrabble
import Sized
import Data.List

-- instance Buffer String where
--   toString     = id
--   fromString   = id
--   line n b     = safeIndex n (lines b)
--   replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
--       where replaceLine' pre [] = pre
--             replaceLine' pre (_:ls) = pre ++ l:ls
--   numLines     = length . lines
--   value        = length . words

instance Buffer (JoinList (Score, Size) String) where

  -- | Convert a buffer to a String.
  toString Empty = ""
  toString (Single m s) = s
  toString (Append m left right) = toString left ++ toString right
  -- | Create a buffer from a String.
  fromString = foldl' (+++) Empty . map (\l -> Single (scoreString l, 1) l) . lines

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> b -> Maybe String
  line = undefined
  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String -> b -> b
  replaceLine = undefined

  -- | Compute the number of lines in the buffer.
  numLines :: b -> Int
  numLines = undefined

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: b -> Int
  value = undefined

