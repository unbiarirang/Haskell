module Collatz where
collatzs :: Integer -> [Integer]
collatzs x
    | x == 0    = error "invalid input"
    | x == 1    = [1]
    | otherwise = x : (collatzs $ f x)

f :: Integer -> Integer
f x
    | x `mod` 2 == 0 = x `div` 2
    | x `mod` 2 == 1 = 3*x + 1
