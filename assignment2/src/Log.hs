-- CIS 194 Homework 2

module Log where

import Control.Applicative
import Data.Char
import Text.Read
import Test.QuickCheck

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

getNumberFromBeginning :: String -> Maybe (Int, String)
getNumberFromBeginning s = case readResult of
  Nothing -> Nothing
  Just int -> Just (int, rest)
  where readResult = readMaybe number
        withoutSpace = dropWhile isSpace s
        (number, rest) = span isDigit withoutSpace

parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType log
  | errorType == "I" = Just (Info, tail log)
  | errorType == "W" = Just (Warning, tail log)
  | errorType == "E" = case maybeErrorCode of
      Nothing -> Nothing
      Just (int, remainingText) -> Just (Error int, remainingText)
  | otherwise = Nothing
  where
    (errorType, afterErrorType) = span (/= ' ') log
    maybeErrorCode = getNumberFromBeginning afterErrorType

parseTimeStamp :: String -> Maybe (TimeStamp, String)
parseTimeStamp s = case readMaybe (takeWhile isDigit (dropWhile isSpace s)) of
  Nothing -> Nothing
  Just int -> Just (int, rest)
  where
    (digit, rest) = span (/= ' ') (dropWhile isSpace s)

maybeParseMessage :: String -> Maybe LogMessage
maybeParseMessage s = do
  (logType, afterType) <- parseMessageType s
  (timeStamp, afterTimestamp) <- parseTimeStamp afterType
  return (LogMessage logType timeStamp (dropWhile isSpace afterTimestamp))


parseMessage :: String -> LogMessage
parseMessage s = case maybeParseMessage s of
  Nothing -> Unknown s
  Just message -> message


parseFile :: String -> [LogMessage]
parseFile s = map parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ tsi _) (Node left x@(LogMessage _ ts _) right)
  | tsi < ts = Node (insert msg left) x right
  | otherwise = Node left x (insert msg right)


build :: [LogMessage] -> MessageTree
build xs = go xs Leaf
  where
    go (x:xs) tree = go xs (insert x tree)
    go [] tree = tree

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right


sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . sortMessages . filter isSevereError
  where
    getMessage (LogMessage _ _ s) = s
