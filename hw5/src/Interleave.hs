module Interleave where
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys

interleaveLists :: [[a]] -> [a]
interleaveLists xxs = foldr1 interleave xxs

{-
interleave' :: [a] -> [a] -> [a]
interleave' (x:xs) (y:ys) = x:y:(interleave' xs ys)
interleave' xs []     = xs
interleave' [] ys     = ys

interleaveLists' :: [[a]] -> [a]
interleaveLists' xxs = foldr1 interleave' xxs
-}
