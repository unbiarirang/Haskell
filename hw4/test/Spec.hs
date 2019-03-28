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


-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
test_1_1 = assertEqual (sort ["5","7","8","9","0"]) (sort $ Brute.getPINs "8")
test_1_2 = assertEqual (sort ["81","82","84","01","02","04"]) (sort $ Brute.getPINs "01")
test_1_3 = assertEqual (sort ["226","228","229","246","248","249","256","258","259","266","268","269","286","288","289","326","328","329","346","348","349","356","358","359","366","368","369","386","388","389","626","628","629","646","648","649","656","658","659","666","668","669","686","688","689"]) (sort $ Brute.getPINs "359")

test_2_1 = assertEqual [100,50,25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] (Collatz.collatzs 100)
test_2_2 = assertEqual [1] (Collatz.collatzs 1)

test_3_1 = assertEqual True (Prefix.isPrefix [1,2] [1,2,3])
test_3_2 = assertEqual True (Prefix.isPrefix [1,2,3,4] [1,2,3])
test_3_3 = assertEqual True (Prefix.isPrefix [1,2,3] [1,2,3])
test_3_4 = assertEqual True (Prefix.isPrefix [] [1])
test_3_5 = assertEqual True (Prefix.isPrefix [1,2,3,4] [1..])
test_3_6 = assertEqual True (Prefix.isPrefix [1..] [1,2,3,4])
test_3_7 = assertEqual False (Prefix.isPrefix [2] [1,2])

test_4_1 = assertEqual (sort [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]) (sort $ Permutations.perms [1,2,3])