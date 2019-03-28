module MyIntList where

import Prelude hiding (length, head, tail, init, last, take, drop)

data IntList = Cons Int IntList | Nil
               deriving (Show)

length :: IntList -> Int
length = \x -> length' x 0
length' :: IntList -> Int -> Int
length' Nil acc         = acc 
length' (Cons x xs) acc = length' xs (acc+1)

head :: IntList -> Int
head Nil         = error "invalid"
head (Cons x _) = x

tail :: IntList -> IntList
tail Nil         = error "invalid"
tail (Cons _ xs) = xs

init :: IntList -> IntList
init Nil          = error "invalid"
init (Cons x Nil) = Nil
init (Cons x xs)  = Cons x (init xs)

last :: IntList -> Int
last Nil          = error "invalid"
last (Cons x Nil) = x
last (Cons _ xs)  = last xs

take :: Int -> IntList -> IntList
take 0 _           = Nil
take _ Nil         = error "invalid"
take n (Cons x xs) = Cons x (take (n-1) xs)

drop :: Int -> IntList -> IntList
drop 0 x           = x
drop _ Nil         = Nil -- error "invalid"
drop n (Cons x xs) = drop (n-1) xs
