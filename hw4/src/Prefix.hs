module Prefix where
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix x y = all (==True) (zipWith (==) x y)
