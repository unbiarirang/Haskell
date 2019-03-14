module GCD where

gcd' :: Integer -> Integer -> Integer
gcd' x y
    | x == 0, y == 0 = 0
    | x `mod` y == 0 = y
    | otherwise = myGCD y (x `mod` y)

myGCD:: Integer -> Integer -> Integer
myGCD = \x y -> gcd' (abs $ max x y) (abs $ min x y)
