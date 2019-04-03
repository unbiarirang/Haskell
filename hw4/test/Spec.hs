{-# OPTIONS_GHC -F -pgmF htfpp #-}
import qualified Brute
import qualified Collatz
import qualified Prefix
import qualified Permutations

import Test.Framework
import System.Timeout(timeout)

import Data.List(sort)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

------------------------------------------------------------------
subst '0' = "08"
subst '1' = "124"
subst '2' = "1235"
subst '3' = "236"
subst '4' = "1457"
subst '5' = "24568"
subst '6' = "3569"
subst '7' = "478"
subst '8' = "57890"
subst '9' = "689"

getPINs :: String -> [String]
getPINs observed = forfor $ map subst observed
-- | forfor [xs1,xs2,...] == [[x1,x2,...] | x1<-xs1, x2<-xs2, ...]
-- | e.g. forfor [[1,2],[3,4,5]] == [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
forfor :: [[a]] -> [[a]]
forfor [] = [[]] -- fallible!
forfor (xs:yss) = [x:zs | x<-xs, zs <- forfor yss]

-- a little more abstract:
-- getPINs = mapM subst
---
collatzs :: Integer -> [Integer]
collatzs x = takeWhilePlus1 (/=1) $ iterate collatz x

collatz :: Integer -> Integer
collatz x = if even x then div x 2 else 3*x+1

takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 _ [] = []
takeWhilePlus1 p (x:xs) = if p x
    then x : takeWhilePlus1 p xs
    else [x]
---
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [inserts x ys | ys <- perms xs]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : [y:rest | rest <- inserts x ys]
---
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix xs ys = and $ zipWith (==) xs ys
------------------------------------------------------------------


-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
test_1_1 = assertEqual (sort $ getPINs "8") (sort $ Brute.getPINs "8")
test_1_2 = assertEqual (sort $ getPINs "01") (sort $ Brute.getPINs "01")
test_1_3 = assertEqual (sort $ getPINs "359") (sort $ Brute.getPINs "359")
test_1_4 = assertEqual (sort $ getPINs "19260817") (sort $ Brute.getPINs "19260817")
test_1_5 = assertEqual (sort $ getPINs $ replicate 10 '6') (sort $ Brute.getPINs $ replicate 10 '6')

test_2_1 = assertEqual (collatzs 100) (Collatz.collatzs 100)
test_2_2 = assertEqual (collatzs 1) (Collatz.collatzs 1)
test_2_3 = assertEqual (collatzs (2^100+1)) (Collatz.collatzs (2^100+1))
test_2_4 = assertEqual (collatzs (2^1000+1)) (Collatz.collatzs (2^1000+1))

test_3_1 = assertEqual True (Prefix.isPrefix [1,2,3] [1,2,3])
test_3_2 = assertEqual True (Prefix.isPrefix [] [1])
test_3_3 = assertEqual True (Prefix.isPrefix [1,2,3,4] [1..])
test_3_4 = assertEqual True (Prefix.isPrefix [1..10^6] [1..10^6+1])
test_3_5 = assertEqual False (Prefix.isPrefix [1..10^6+1] ([1..10^6]++[1]))
test_3_6 = assertEqual True (Prefix.isPrefix [1..] [1..10^6+1])
test_3_7 = assertEqual False (Prefix.isPrefix [1..] ([1..10^6]++[1]))

test_4_1 = assertEqual (sort $ perms [1,2,3]) (sort $ Permutations.perms [1,2,3])
test_4_2 = assertEqual (sort $ perms [1..5]) (sort $ Permutations.perms [1..5])
test_4_3 = assertEqual (sort $ perms [1..7]) (sort $ Permutations.perms [1..7])
test_4_4 = assertEqual (sort $ perms [1..8]) (sort $ Permutations.perms [1..8])
-- 8 171ms; 9 2755ms