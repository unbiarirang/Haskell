module Permutations where
perms :: [a]-> [[a]]
perms [] = []
perms (x:xs) = insertAllList [x] (perms xs)

insertAllList :: [a] -> [[a]] -> [[a]]
insertAllList c [] = [c]
insertAllList c (x:xs)
    | length xs == 0 = insertAll c x
    | otherwise      = (insertAll c x) ++ insertAllList c xs 

insertAll :: [a] -> [a] -> [[a]]
insertAll c list =
    zipWith insert (replicate (length list + 1) (c, list)) [0..]

-- insert the first elem into the second list at the position of the idx
-- insert ([1], [2,3]) 1 => [2,1,3]
insert :: ([a], [a]) -> Int -> [a]
insert (x, []) 0 = x
insert (x, (y:ys)) idx
    | idx == 0   = x++y:ys
    | otherwise  = y: insert (x, ys) (idx-1)
