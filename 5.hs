{-
    2520 is the smallest number that can be divided by each of the numbers from
    1 to 10 without any remainder.

    What is the smallest positive number that is evenly divisible by all of the
    numbers from 1 to 20?
-}

divisibleByAll :: Integral a => a -> [a] -> Bool
divisibleByAll x = all (\y -> x `mod` y == 0)

result = head $ filter (flip divisibleByAll [20,19..1]) [2520..]
-- 232792560