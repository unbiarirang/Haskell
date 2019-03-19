{-# OPTIONS_GHC -F -pgmF htfpp #-}
import MyIntList
import MyList
import Poemistry

import Test.Framework
import System.Timeout(timeout)


-- TODO in test report, Failure, not expected timeout 2019-03-06
addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests -- magical preprocessing! 2019-03-06

main = htfMain testsWithTimeouts

closeEnough :: Double -> Double -> Bool
closeEnough actual expect = abs (actual-expect) < 1e-6

-- assertEqual as preprocessor macro https://hackage.haskell.org/package/HTF-0.13.2.2/docs/Test-Framework-HUnitWrapper.html
il = Cons 1 (Cons 2 (Cons 3 MyIntList.Nil))
test_1_1 = assertEqual 3 (MyIntList.length il)
test_1_2 = assertEqual 0 (MyIntList.length MyIntList.Nil) 
test_1_3 = assertEqual 1 (MyIntList.head il)
test_1_4 = assertEqual (show $ Cons 2 (Cons 3 MyIntList.Nil)) (show $ MyIntList.tail il)
test_1_5 = assertEqual (show MyIntList.Nil) (show $ MyIntList.tail (Cons 1 MyIntList.Nil))
test_1_6 = assertEqual (show $ Cons 1 (Cons 2 MyIntList.Nil)) (show $ MyIntList.init il)
test_1_7 = assertEqual 3 (MyIntList.last il)
test_1_8 = assertEqual (show $ Cons 1 (Cons 2 MyIntList.Nil)) (show $ MyIntList.take 2 il)
test_1_9 = assertEqual (show MyIntList.Nil) (show $ MyIntList.take 0 il)
test_1_10 = assertEqual (show il) (show $ MyIntList.drop 0 il)

l1 = 'a':~'b':~MyList.Nil
l2 = 'c':~MyList.Nil
l3 = MyList.Nil
l4 = 'd':~'e':~MyList.Nil
test_2_1 = assertEqual (show $ 'a' :~ ('b' :~ ('c' :~ ('d' :~ ('e' :~ MyList.Nil))))) (show $ MyList.concat $ l1:~l2:~l3:~l4:~MyList.Nil)
--test_2_2 = assertEqual "Nil" (show $ MyList.concat MyList.Nil)
--test_2_3 = assertEqual "Nil" (show $ MyList.concat $ MyList.Nil :~ MyList.Nil)

test_3_1 = assertEqual "\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834\21834" (poemistry ['啊','唉'] 0)
test_3_2 = assertEqual "啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊" (poemistry ['啊','唉'] 0)
test_3_3 = assertEqual "\21834\21834\21834\21834\21834\n\21834\21834\21834\21834\21834\n\21834\21834\21834\21834\21834\n\21834\21834\21834\21834\21769\n" (prettyPrint $ poemistry ['啊','唉'] 1)
test_3_4 = assertEqual "啊啊啊啊啊\n啊啊啊啊啊\n啊啊啊啊啊\n啊啊啊啊唉\n" (prettyPrint $ poemistry ['啊','唉'] 1)
test_3_5 = assertEqual "明月是故乡\n故乡明月是\n是故乡明月\n月是故乡明\n" (prettyPrint $ poemistry ['月','是','故','乡','明'] 77470471781444)
