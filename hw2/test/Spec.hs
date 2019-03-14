{-# OPTIONS_GHC -F -pgmF htfpp #-}
import GCD(myGCD) 
import BinTree
import BinaryHeight(height)
import BinaryBalance(isBalance)

import Test.Framework
import System.Timeout(timeout)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html

test_1_1 = assertEqual 2147483648 (myGCD (-10^30+2^30) (10^100-2^31))
test_1_2 = assertEqual 4 (myGCD 0 (-4))
test_1_3 = assertEqual 0 (myGCD 0 0)

t1 = Nil
t2 = Node (Node Nil Nil 0) Nil 0
t3 = Node t1 t2 0
t4 = Node t3 t3 0
t5 = Node t2 t2 0

test_2_1 = assertEqual 0 (height t1)
test_2_2 = assertEqual 2 (height t2)
test_2_3 = assertEqual 3 (height t3)
test_2_4 = assertEqual 4 (height t4)

test_3_1 = assertEqual True (isBalance t1)
test_3_2 = assertEqual True (isBalance t2)
test_3_3 = assertEqual False (isBalance t3)
test_3_4 = assertEqual False (isBalance t4)
test_3_5 = assertEqual True (isBalance t5)
