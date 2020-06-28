gmodule SundaramSpec where

import Test.Hspec
import Test.QuickCheck
import Sundaram

primes = 2 : sieve [3,5..]
  where sieve (p:xs) = p : (sieve $ filter (notDiv p) xs)
        notDiv p n = n `mod` p /= 0

getPrimesReference :: Int -> [Int]
getPrimesReference n = take n primes


spec :: Spec
spec = do
  describe "odd prime numbers" $ do
    it "gives same result as reference implementation" $ do
      let sud = sieveSundaram 100 in
        let primes = getPrimesReference ((length sud) + 1) in
         tail primes `shouldBe` map fromIntegral sud
