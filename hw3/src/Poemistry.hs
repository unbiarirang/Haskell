module Poemistry where

poemistry :: [Char] -> Integer -> [Char]
poemistry dict k =
    let base = length dict;
        idxs = getPadding $ reverse $ toBase base (fromIntegral k);
    in map (getChar dict) idxs
    where getChar dict idx = dict !! idx
          getPadding idxs
              | length idxs < 20 = getPadding $ 0 : idxs
              | otherwise = idxs
          toBase bs x
              | x < bs    = [x]
              | otherwise = let (q,r) = x `divMod` bs 
                            in r : (toBase bs q)

prettyPrint :: [Char] -> [Char]
prettyPrint str
    | length str == 0 = ""
    | otherwise =
        let x = take 5 str; xs = drop 5 str;
        in x ++ "\n" ++ prettyPrint xs
    

recurse bs x
    | x < bs    = [x]
    | otherwise = let (q,r) = x `divMod` bs 
                   in r : (recurse bs q)


--import Data.Char (intToDigit, toUpper)
--
--toHex :: Int -> String
--toHex = map toUpper . reverse . recurse
--  where recurse n
--          | n < 16    = [ intToDigit n ]
--          | otherwise = let (q,r) = n `divMod` 16
--                        in (intToDigit r) : recurse q
