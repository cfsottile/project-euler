{-
    A palindromic number reads the same both ways. The largest palindrome made 
    from the product of two 2-digit numbers is 9009 = 91 × 99.

    Find the largest palindrome made from the product of two 3-digit numbers.
-}

isPalindrome :: (Show a, Integral a) => a -> Bool
isPalindrome x = reverse (show x) == show x

palsProductOf3Digits :: [Int]
palsProductOf3Digits = [ a * b | a <- ([999,998..100] :: [Int]), b <- ([999,998..100] :: [Int]), isPalindrome (a * b) ]

palsProductOf3Digits' :: [Int]
palsProductOf3Digits' = do
    a <- [999,998..100] :: [Int]
    b <- [999,998..100] :: [Int]
    if isPalindrome (a * b) then return (a * b) else []

result = maximum palsProductOf3Digits