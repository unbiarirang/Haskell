{-# OPTIONS_GHC -F -pgmF htfpp #-}
import MyListType(List(..))
import MyList
import ChurchNumerals(c0,cSucc,cToInt,cPlus,cMult,cExp)

import Test.Framework -- dependencies: HTF
import System.Timeout(timeout)

import Data.List(elemIndex) -- simpler than findIndex
import Data.Maybe(fromJust)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

t0 :: List Int
t0 = Nil
t1 = 1:~Nil
t2 = 1:~2:~Nil
t3 = 1:~2:~3:~Nil
t3' = 1:~2:~4:~Nil
t3'' = 2:~1:~1:~Nil
map' f Nil = Nil; map' f (x:~xs) = f x :~ map' f xs
tn = 1 :~ map' (\x->x-1) tn 

e2 = t2 == 1:~2:~Nil
e3 = t1 /= t0
e4 = t1 == tn
e5 = t3 == t3'

o1 = tn < t1 -- hack the non-exhaustive pattern matching solution
o2 = t1 < t1
o3 = t3' > t3
o4 = t3' < tn

s1 = show t0
s2 = show t3''
s3 = show t3'
s4 = show t3

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
test_1_1_1 = assertEqual False e5
test_1_1_2 = assertEqual True e2
test_1_1_3 = assertEqual True e3
test_1_1_4 = assertEqual False e4 

test_1_2_1 = assertEqual False o1
test_1_2_2 = assertEqual False o2
test_1_2_3 = assertEqual True o3
test_1_2_4 = assertEqual False o4

test_1_3_1 = assertEqual "[]" s1
test_1_3_2 = assertEqual "[2,1,1]" s2
test_1_3_3 = assertEqual "[1,2,4]" s3
test_1_3_4 = assertEqual "[1,2,3]" s4

------------------------------------------------------------------------------------------------------------------------------------------------------

c2 = \f -> \x -> f (f x)
c3 = \f -> \x -> f (f (f x))
c4 = \f -> \x -> f (f (f (f x)))

mycToInt cn = cn (\x -> x + 1) 0

test_2_1_1 = assertEqual 0 $ mycToInt c0   --10 points
test_2_1_2 = assertEqual 0 $ mycToInt c0   --dummy

test_2_2_1 = assertEqual 3 $ cToInt c3   --5 points
test_2_2_2 = assertEqual 4 $ cToInt c4   --5 points

test_2_3_1 = assertEqual 4 $ mycToInt $ cSucc c3   --5 points
test_2_3_2 = assertEqual 5 $ mycToInt $ cSucc c4   --5 points

test_2_4_1 = assertEqual 6 $ mycToInt $ cPlus c2 c4  --5 points
test_2_4_2 = assertEqual 7 $ mycToInt $ cPlus c3 c4  --5 points

test_2_5_1 = assertEqual 12 $ mycToInt $ cMult c4 c3  --5 points
test_2_5_2 = assertEqual 8 $ mycToInt $ cMult c4 c2  --5 points

test_2_6_1 = assertEqual 64 $ mycToInt $ cExp c4 c3 --5 points
test_2_6_2 = assertEqual 256 $ mycToInt $ cExp c4 c4 --5 points
