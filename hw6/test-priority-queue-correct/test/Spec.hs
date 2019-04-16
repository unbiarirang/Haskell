import Test.QuickCheck
import PriorityQueue(PriorityQueue(..))
import BHeap(BHeap(..))
import Data.List(sort, group)
-- You may import more

main :: IO ()
main = do
    -- example, weak properties
    quickCheck prop_1_empty_is_empty
    quickCheck prop_11_not_empty
    quickCheck prop_2_findMin_the_only_element
    quickCheck prop_12_deleteMin_the_only_element
    quickCheck prop_3_tautology
    quickCheck prop_4_insert_max
    quickCheck prop_5_insert_min
    quickCheck prop_6_findMin
    quickCheck prop_7_deleteMin
    quickCheck prop_8_meld_then_findMin
    quickCheck prop_9_deleteMin_multiple_times
    quickCheck prop_10_meld_with_empty_queue
    quickCheck prop_11_delete_all_then_empty
    quickCheck prop_12_delete_all_but_one
    quickCheck prop_13_insert_delete_n_times_then_empty
    quickCheck prop_14_find_and_delete_all
    quickCheck prop_15_merge_find_and_delete_all
    quickCheck prop_16_compare_insertall_and_merge
    -- quickCheck or verboseCheck more properties here!

-- 1. Empty queue should be empty
prop_1_empty_is_empty :: Bool
prop_1_empty_is_empty = isEmpty empty_BHeap_of_Integer

empty_BHeap_of_Integer :: BHeap Integer
empty_BHeap_of_Integer = empty

-- After inserting a Integer, a queue should be not empty
prop_11_not_empty :: Integer -> Bool
prop_11_not_empty x =
    (isEmpty $ insert x (empty :: BHeap Integer)) == False

-- 2. For all integer n, insert n to an empty priority queue, then findMin from it, the result should be n
prop_2_findMin_the_only_element :: Integer -> Bool
prop_2_findMin_the_only_element n = findMin s == n where
    s = insert n empty_BHeap_of_Integer

-- After deleting an element from the queue having sole item, it should be empty
prop_12_deleteMin_the_only_element :: Integer -> Bool
prop_12_deleteMin_the_only_element n =
    (isEmpty $ deleteMin (fromList [n] :: BHeap Integer)) == True

-- 3. For all integer n, for all non-empty heap h, either n <= findMin h or n > findMin h.
-- This is a taotology, only to demostrate how to write a property containing implication (==>) and multiple random inputs.
prop_3_tautology :: Integer -> BHeap Integer -> Property
prop_3_tautology n h = not (isEmpty h) ==>
    n <= findMin h || n > findMin h

prop_4_insert_max :: [Integer] -> Property
prop_4_insert_max l = length rmduped >= 2 ==>
    findMin (insert m (fromList rmduped :: BHeap Integer)) /= m
    where rmduped = rmdups l
          m = maximum rmduped

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

prop_5_insert_min :: [String] -> Property
prop_5_insert_min l = not (null l) ==>
    findMin (insert m (fromList l :: BHeap String)) == m
    where m = minimum l

prop_6_findMin :: BHeap Integer -> Property
prop_6_findMin h = not (isEmpty h) ==>
    findMin h == (minimum $ toList h)

prop_7_deleteMin :: [Char] -> Property
prop_7_deleteMin l = length l >= 2 ==>
    findMin (deleteMin $ fromList l :: BHeap Char)
        == findMin (fromList $ removeItem (minimum l) l :: BHeap Char)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys

prop_8_meld_then_findMin :: BHeap String -> BHeap String -> Property
prop_8_meld_then_findMin h1 h2 = not (isEmpty h1 || isEmpty h2) ==>
    min (findMin h1) (findMin h2) == (findMin $ meld h1 h2)

prop_9_deleteMin_multiple_times :: [Integer] -> Property
prop_9_deleteMin_multiple_times l = length l >= 3 ==>
    all (==True) [
        findMin h == head sorted,
        (findMin $ deleteMin h) == (head $ drop 1 sorted),
        (findMin $ deleteMin $ deleteMin h) == (head $ drop 2 sorted)
    ] == True
    where h = fromList l :: BHeap Integer
          sorted = sort l

prop_10_meld_with_empty_queue :: BHeap String -> Property
prop_10_meld_with_empty_queue h = not (isEmpty h) ==>
    all (==True) [
        toList (meld h empty :: BHeap String) == toList h,
        toList (meld (empty :: BHeap String) h) == toList h
    ] == True

prop_11_delete_all_then_empty :: [Integer] -> Bool
prop_11_delete_all_then_empty l =
    (isEmpty $ foldl (\acc _ -> deleteMin acc) h l)
    == True
    where h = fromList l :: BHeap Integer

prop_12_delete_all_but_one :: [Char] -> Property
prop_12_delete_all_but_one l = not (null l) ==>
    (findMin $ foldl (\acc _ -> deleteMin acc) h (tail l))
    == maximum l
    where h = fromList l :: BHeap Char

prop_13_insert_delete_n_times_then_empty :: [Char] -> Property
prop_13_insert_delete_n_times_then_empty l = not (null l) ==>
    (isEmpty $ foldl (\acc _ -> deleteMin acc) inserted l) == True
    where e = empty :: BHeap Char
          inserted = foldl (\acc x -> insert x acc) e l

prop_14_find_and_delete_all :: [Char] -> Property
prop_14_find_and_delete_all l = not (null l) ==>
    all (==True) [
        isEmpty deleted == True,
        newList == (reverse $ sort l)
    ] == True
    where h = fromList l :: BHeap Char
          (deleted, newList) = foldl (\(acc,l) _ -> (deleteMin acc, findMin acc : l)) (h, []) l

prop_15_merge_find_and_delete_all :: [Integer] -> [Integer] -> Property
prop_15_merge_find_and_delete_all l1 l2 = not (null l1 || null l2) ==>
    all (==True) [
        isEmpty deleted == True,
        newList == (reverse $ sort $ l1 ++ l2)
    ] == True
    where h1 = fromList l1 :: BHeap Integer
          h2 = fromList l2 :: BHeap Integer
          merged = meld h1 h2
          (deleted, newList) = foldl (\(acc,l) _ -> (deleteMin acc, findMin acc : l)) (merged, []) (l1 ++ l2)

prop_16_compare_insertall_and_merge :: BHeap Integer -> [Integer] -> Property
prop_16_compare_insertall_and_merge h l = not (isEmpty h || null l) ==>
    findMin inserted == findMin merged
    where inserted = insertAll l h
          merged = meld h (fromList l :: BHeap Integer)

-- | Generator of @BHeap a@, used to generate random @BHeap@s
instance (Arbitrary a, Ord a) => Arbitrary (BHeap a) where
    arbitrary = do
        avs <- arbitrary -- :: Gen [a] -- see also @vector :: Arbitrary a => Int -> Gen [a]@
        return (fromList avs)
