module Bachelor where
import Data.Bits

bachelor :: [Integer] -> Integer
bachelor = foldl1 xor

{-
-import Data.List
-bachelor :: [Integer] -> Integer
-bachelor l = fst $ head $ dropWhile (\(x1, x2) -> x1 == x2) $ combine $ sort l
-
-combine :: [Integer] -> [(Integer, Integer)]
-combine (x1:x2:xs) = (x1, x2) : combine xs
-combine (x1:x2) = [(x1, 0)]
-}

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]
