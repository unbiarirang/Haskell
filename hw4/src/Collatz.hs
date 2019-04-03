module Collatz where
collatzs :: Integer -> [Integer]
collatzs x
    | x == 0    = error "invalid input"
    | x == 1    = [1]
    | otherwise = x : (collatzs $ f x)

f :: Integer -> Integer
f x
    | even x = x `div` 2
    | odd x  = 3 * x + 1
