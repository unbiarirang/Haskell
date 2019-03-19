{-# OPTIONS_GHC -F -pgmF htfpp #-}
import BinaryHeight(height)
import BinaryBalance(isBalance)
import GCD(myGCD)
import qualified BinTree

import Test.Framework
import System.Timeout(timeout)


-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html

test_1_1 = assertEqual (gcd 0 100) (GCD.myGCD 0 100)
test_1_2 = assertEqual (gcd 100 (-100)) (GCD.myGCD 100 (-100))
test_1_3 = assertEqual (gcd (-2^70) (3^60)) (GCD.myGCD (-2^70) (3^60))
test_1_4 = assertEqual (gcd 0 0) (GCD.myGCD 0 0)

test_2_1 = assertEqual 0 (height BinTree.Nil)
test_2_2 = assertEqual 2 (height (BinTree.Node (BinTree.Node BinTree.Nil BinTree.Nil 0) BinTree.Nil 0))
test_2_3 = assertEqual 2 (height (BinTree.Node (BinTree.Node BinTree.Nil BinTree.Nil 0) (BinTree.Node BinTree.Nil BinTree.Nil 0) 0)) 

test_3_1 = assertEqual True (isBalance BinTree.Nil)
test_3_2 = assertEqual True (isBalance (BinTree.Node (BinTree.Node BinTree.Nil BinTree.Nil 0) BinTree.Nil 0))
test_3_3 = assertEqual False (isBalance (BinTree.Node (BinTree.Node (BinTree.Node BinTree.Nil BinTree.Nil 0) BinTree.Nil 0) BinTree.Nil 0))