module Fib where


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 = [0, 1] ++ zipWith (+) fibs2 (tail fibs2)


-- TODO: https://www.cis.upenn.edu/~cis194/spring13/hw/06-laziness.pdf Exercise 6
-- It seems really interesting but it's a longer project
