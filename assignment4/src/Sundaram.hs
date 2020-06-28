module Sundaram where

sieveSundaram :: Integer -> [Integer]
sieveSundaram n=[2*x+1| x <-[1..n],not(elem x[i+j+2*i*j|i<-[1..n],j<-[i..n]])]
