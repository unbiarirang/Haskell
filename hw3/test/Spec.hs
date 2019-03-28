{-# OPTIONS_GHC -F -pgmF htfpp #-}
import MyIntList(IntList(..),length, head, tail, init, last, take, drop)
import MyList(List(..),concat)
import Prelude hiding (length, head, tail, init, last, take, drop, concat)
import Poemistry(poemistry, prettyPrint)

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
l1 = 'a':~'b':~MyList.Nil
l2 = 'c':~MyList.Nil
l3 = MyList.Nil
l4 = 'd':~'e':~MyList.Nil

instance Eq IntList where
    MyIntList.Nil == MyIntList.Nil = True
    Cons x xs == Cons y ys = x == y && xs == ys

instance Eq a => Eq (List a) where
    MyList.Nil == MyList.Nil = True
    (x:~xs) == (y:~ys) = x == y && xs == ys

-- assertEqualFlip = flip assertEqual -- no. preprocessing + infix = boom

test_1_1 = assertEqual (3 :: Int) $ length il
test_1_2 = assertEqual (1 :: Int) $ head il
test_1_3 = assertEqual (Cons 2 (Cons 3 MyIntList.Nil)) $ tail il
-- test_1_3 = assertEqual tail il `assertEqualFlip` (Cons 2 (Cons 3 MyIntList.Nil))
test_1_4 = assertEqual (Cons 1 (Cons 2 MyIntList.Nil)) $ init il
test_1_5 = assertEqual (3 :: Int) $ last il
test_1_6 = assertEqual (Cons 1 (Cons 2 MyIntList.Nil)) $ take 2 il
test_1_7 = assertEqual (Cons 3 MyIntList.Nil) $ drop 2 il
test_1_8 = assertEqual (0 :: Int) $length MyIntList.Nil
test_1_9 = assertEqual (MyIntList.Nil) $ take 0 il
test_1_10 = assertEqual (Cons 3 MyIntList.Nil) $ drop 2 il

test_2_1 = assertEqual ('a' :~ ('b' :~ ('c' :~ ('d' :~ ('e' :~ MyList.Nil))))) $ (concat $ l1:~l2:~l3:~l4:~MyList.Nil)
test_2_2 = assertEqual (MyList.Nil :: List ()) $ (concat MyList.Nil :: List ())
test_2_3 = assertEqual ('a':~'b':~MyList.Nil) $ (concat $ l1:~MyList.Nil)


test_3_1 = assertEqual "啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊啊" $ poemistry ['啊','唉'] 0
test_3_2 = assertEqual "明月是故乡故乡明月是是故乡明月月是故乡明" $ poemistry ['月','是','故','乡','明'] 77470471781444
test_3_3 = assertEqual "12345\n22345\n32345\n42345\n" $ prettyPrint "12345223453234542345"
