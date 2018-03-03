{-
    The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143 ?
-}

primeFactors :: Integral a => a -> [a]
primeFactors x = if f < x then f : primeFactors (x `div` f) else [x]
    where f = firstPrimeFactor x

firstPrimeFactor :: Integral a => a -> a
firstPrimeFactor 1 = 1
firstPrimeFactor x = head $ filter (\f -> x `mod` f == 0) [2..]

property :: Integral a => a -> Bool
property x = foldl (*) 1 (primeFactors x) == x

result = last $ primeFactors 600851475143