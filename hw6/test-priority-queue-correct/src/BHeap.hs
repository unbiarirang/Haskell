-- |An almost totally wrong Binomial Heap implementation of priority queue.
-- However, the example properties (@prop_1_empty_is_empty, prop_2_findMin_the_only_element, prop_3_tautology@) in @test/Spec.hs@ are too weak to detect bugs from this implementation.
--
-- Write more, critical properties in @test/Spec.hs@ to detect bugs from this implementation AND more subtle bugs in other possible implementations!
module BHeap where

import PriorityQueue
import HeapNode
import Data.List(sort)

newtype BHeap a = BHeap [Node a] deriving (Show)

insertHead x (BHeap xs) = BHeap (x:xs)

instance PriorityQueue BHeap where
  -- | Correct
  empty = BHeap []
  -- | Correct
  isEmpty (BHeap l) = null l
  --insert n (BHeap []) = BHeap (Node {value=n, rank=0, children=[]}:[])
  --insert n (BHeap all@(x:xs))
  --    | n <= (value x) = BHeap (Node {value=n, rank=0, children=[]}:all)
  --    | n >  (value x) = insertHead x (insert n (BHeap xs))
  insert x (BHeap xs) = BHeap (Node {value=x, rank=0, children=[]}:xs)
  -- | Wrong. Should place the elements to well-designed places.
  meld (BHeap xs) (BHeap ys) = BHeap (xs ++ ys)
  --meld (BHeap xs) (BHeap []) = BHeap xs
  --meld (BHeap []) (BHeap ys) = BHeap ys
  --meld (BHeap xall@(x:xs)) (BHeap yall@(y:ys))
  --    | (value x) <= (value y) = insertHead x (meld (BHeap xs) (BHeap yall))
  --    | otherwise = insertHead y (meld (BHeap xall) (BHeap ys))
  -- | Correct.
  -- findMin (BHeap xs) = value $ head xs
  findMin (BHeap xs) = foldl (\acc n -> if acc <= (value n) then acc else (value n)) (value $ head xs) xs
  -- | Correct.
  deleteMin (BHeap xs) = BHeap $ (takeWhile (\n -> value n /= m) xs) ++ tail (dropWhile (\n -> value n /= m) xs)
      where m = findMin $ BHeap xs
