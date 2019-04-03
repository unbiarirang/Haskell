module Interleave where
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave xs [] = xs
interleave [] ys = ys

interleaveLists :: [[a]] -> [a]
interleaveLists xxs = foldr1 interleave xxs

{-
interleave' :: [a] -> [a] -> [a]
interleave' (x:xs) []     = x:xs
interleave' [] (y:ys)     = y:ys
interleave' (x:xs) (y:ys) = x:y:(interleave' xs ys)

interleave'' :: [a] -> [a] -> [a]
interleave'' x [] = x
interleave'' [] y = y
interleave'' (x:xs) (y:ys)
    | x < y = x:interleave'' xs (y:ys)
    | otherwise = y:interleave'' (x:xs) ys

interleaveLists' xxs = foldr1 interleave' xxs
interleaveLists'' xxs = foldr1 interleave'' xxs

intPairs = [ [ (x, y) | y <- [1..] ] | x <- [1..] ]
-}
