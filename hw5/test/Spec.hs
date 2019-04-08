{-# OPTIONS_GHC -F -pgmF htfpp #-}
import qualified Bachelor
import qualified Interleave
import qualified MaxSubstrSum

import Test.Framework
import System.Timeout(timeout)

import Data.List(elemIndex) -- simpler than findIndex
import Data.Maybe(fromJust)

import System.IO.Unsafe
import Control.DeepSeq -- Besides HTF, you also need to add deepseq to package.yaml dependencies to use this module!

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = prepare `deepseq` htfMain testsWithTimeouts
prepare = [xs15,xs16,xs31,xs34]

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6


-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
---------------------------------------------------------------------------------------------------
test_1_1 = assertEqual 2 (Bachelor.bachelor [1,1,2])
test_1_2 = assertEqual 2 (Bachelor.bachelor [2,3,3,6,6])
test_1_3 = assertEqual 1 (Bachelor.bachelor [1])
test_1_4 = assertEqual 9 (Bachelor.bachelor (9:[2..10000] ++ [2..10000]))
test_1_5 = assertEqual 233333333333333333 (Bachelor.bachelor xs15)
test_1_6 = assertEqual 2333333333333333333 (Bachelor.bachelor xs16)

xs15 :: [Integer]
xs15 = 233333333333333333 : [2 .. 1000000] ++ [2 .. 1000000]
xs16 :: [Integer]
xs16 = 2333333333333333333 : [2 .. 10000000] ++ [2 .. 10000000]
---------------------------------------------------------------------------------------------------
xs1 = [1,2]
ys1 = [3,4]
x1 = 4
test_2_1 = assertEqual True $ fromJust (elemIndex x1 $ Interleave.interleave xs1 ys1) >= 0

xs2 = [1,2]
ys2 = [3..]
x2 = 10^7-3
test_2_2 = assertEqual True $ fromJust (elemIndex x2 $ Interleave.interleave xs2 ys2) >= 0

daytimes = ("D",1) : map (\(x,y)->(x,y+1)) daytimes
nights = ("N",1) : map (\(x,y)->(x,y+1)) nights
daysAndNights = Interleave.interleave daytimes nights
x3 = ("D",10^6)
test_2_3 = assertEqual True $ fromJust (elemIndex x3 daysAndNights) >= 0


intPairs = Interleave.interleaveLists [ [ (x, y) | y <- [1..] ] | x <- [1..] ]
x4 = (5,5)
x5 = (10,10)
x6 = (13,13)
test_2_4 = assertEqual True $ fromJust (elemIndex x4 intPairs) >= 0
test_2_5 = assertEqual True $ fromJust (elemIndex x5 intPairs) >= 0
test_2_6 = assertEqual True $ fromJust (elemIndex x6 intPairs) >= 0

test_2_7 = assertEqual True $ fromJust (elemIndex (2*10^7) $ Interleave.interleave [1,3..] [2,4..]) >= 0

x8 = (1,1)
test_2_8 = assertEqual True $ fromJust (elemIndex x8 intPairs) >= 0
--------------------------------------------------------------------------------------------------------
-- test_3_1 = assertEqual 1 (MaxSubstrSum.solution [-1,1])
test_3_1 = assertEqual 4988612 (MaxSubstrSum.solution xs31)
test_3_2 = assertEqual 7 (MaxSubstrSum.solution [-1,-1,1,1,2,3])
test_3_3 = assertEqual 204 (MaxSubstrSum.solution (1:2:(-3):[4..20]))
test_3_4 = assertEqual 500000500000 (MaxSubstrSum.solution xs34)
test_3_5 = assertEqual 6 (MaxSubstrSum.solution ([0,(-1)..(-10000)]++[1,2,3]))
test_3_6 = assertEqual 0 (MaxSubstrSum.solution [-1,-1,-2,-3,-5])

xs31IO :: IO [Integer]
xs31IO = do
    xstr <- readFile "test/hw5-xs31.txt"
    readIO xstr :: IO [Integer]
xs31 :: [Integer]
xs31 = unsafePerformIO xs31IO

xs34 :: [Integer]
xs34 = [1..10^6]

--------------------------------------------------------------------------------------------------------
