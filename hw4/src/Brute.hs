module Brute where

getPINs :: String -> [String]
getPINs observed
    | observed == ""        = []
    | length observed == 1  = sep nums
    | otherwise             = getSet nums
    where nums = map getPIN observed

getSet :: [String] -> [String]
getSet (x:xs:xss)
    | xss == []     = combine (sep $ rp x $ length xs) (sep $ rp2 [xs] (length x))
    | otherwise     = combine (sep $ rp x $ length y) (rp2 y $ length x)
    where y = getSet $ xs : xss

-- sep ["11", "22"] => ["1","1","2","2"]
sep :: [String] -> [String]
sep str = concat $ map (map (:"")) str 

-- rp "123" 3 => ["111", "222", "333"]
rp :: String -> Int -> [String]
rp str n = map (replicate n) str

-- rp2 ["12", "34"] 2 => ["12", "34", "12", "34"]
rp2 :: [String] -> Int -> [String]
rp2 xs n = take (n * length xs) $ f xs
    where f x = x ++ f x

combine :: [String] -> [String] -> [String]
combine a b = zipWith (++) a b

getPIN :: Char -> String
getPIN c = case c of
    '0' -> "08"
    '1' -> "124"
    '2' -> "1235"
    '3' -> "236"
    '4' -> "1457"
    '5' -> "24568"
    '6' -> "3569"
    '7' -> "478"
    '8' -> "57890"
    '9' -> "689"
    otherwise -> error "invalid input"
