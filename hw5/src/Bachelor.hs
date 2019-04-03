module Bachelor where
import Data.List
bachelor :: [Integer] -> Integer
bachelor l = fst $ head $ dropWhile (\(x1, x2) -> x1 == x2) $ combine $ sort l

combine :: [Integer] -> [(Integer, Integer)]
combine (x1:x2:xs) = (x1, x2) : combine xs
combine (x1:x2) = [(x1, 0)]

{-
bachelor' :: [Integer] -> Integer
bachelor' l = head $ foldl' f [] l
bachelor'' :: [Integer] -> Integer
bachelor'' l = fst $ foldl' ff (0,0) $ sort l
bachelor''' :: [Integer] -> Integer
bachelor''' l = foldl' fff 0 $ combine $ sort l

f acc x
    | elem x acc = removeItem x acc
    | otherwise  = x : acc

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

ff :: (Integer, Integer) -> Integer -> (Integer, Integer)
ff (x1,x2) x
    | x2 == -1           = (x1, -1)
    | x2 == 0            = (x, 1)
    | x2 == 1 && x1 == x = (x, 0)
    | otherwise          = (x1, -1)

fff :: Integer -> (Integer, Integer) -> Integer
fff x (x1,x2)
    | x /= 0    = x
    | x1 == x2  = 0
    | otherwise = x1
-}
