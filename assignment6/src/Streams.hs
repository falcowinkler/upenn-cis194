module Streams where

data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a:(streamToList rest)

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rest)  = Cons (f a) (streamMap f rest)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =  Cons a (streamFromSeed f (f a))


nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x rest) xs = Cons x (interleaveStreams xs rest)


ruler :: Stream Integer
ruler = go 0
  where go n = interleaveStreams (streamRepeat n) (go (n+1))
