{-# OPTIONS_GHC -F -pgmF htfpp #-}
import qualified APlusB
import qualified LCM
import qualified Distance

import Test.Framework
import System.Timeout(timeout)

-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html

test_1_1 = assertEqual 4 (APlusB.solution 2 2)
test_1_2 = assertEqual 1 (APlusB.solution (-1) 2)

test_2_1 = assertEqual 42 (LCM.solution 6 7)
test_2_2 = assertEqual 24 (LCM.solution 6 8)
test_2_3 = assertEqual 50046646252225799669066498328345194358436710580224 (LCM.solution (2^70) (3^60))

test_3_1 = assertEqual 2 (Distance.solution 1 (1,2) (2,3))
test_3_2 = assertEqual (sqrt 2) (Distance.solution 2 (1,2) (2,3))
test_3_3 = assertEqual 2 (Distance.solution 3 (1,2) (2,4))
test_3_4 = assertEqual 2 (Distance.solution 998 (1,2) (2,4))
test_3_5 = assertEqual 0 (Distance.solution 998 (1,2) (1,2))
