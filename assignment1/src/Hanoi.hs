module Hanoi where

--To move n discs (stacked in increasing size) from peg a to peg b
--using peg c as temporary storage,
--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b
                ++
                [(a, b)]
                ++
                hanoi (n - 1) c b a

-- move discs from peg 1 to peg 2 using peg3 and 4 as auxilaries
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d =
    --transfer the top k disks to a single peg other than the start or destination pegs
    hanoi4 k a c b d ++
    -- Without disturbing the peg that now contains the top k disks, transfer the remaining n-k disks to the destination peg
    hanoi (n - k) a b d ++
    -- Finally, transfer the top k disks to the destination peg
    hanoi4 k c b a d
    -- optimal k, from wikipedia
    where k = n - (round (sqrt (fromIntegral (2 * n + 1)))) + 1
