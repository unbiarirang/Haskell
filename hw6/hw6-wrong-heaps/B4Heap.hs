module BHeap where

import PriorityQueue
import HeapNode

-- | Buggy Binomial Heap implementation 4
newtype BHeap a = BHeap [Node a] deriving (Show)

-- | Correct
link :: (Ord a) => Node a -> Node a -> Node a
link t1 t2 = if value t1 <= value t2
  then Node { value=value t1,
              rank=rank t1 + 1,
              children=t2:children t1 }
  else Node { value=value t2,
              rank=rank t2 + 1,
              children=t1:children t2 }

-- | Correct
ins4 :: (Ord a) => Node a -> BHeap a -> BHeap a
ins4 t (BHeap []) = BHeap [t]
ins4 t (BHeap (tp:ts)) = if rank t < rank tp
                        then BHeap (t:tp:ts)
                        else ins4 (link t tp) (BHeap ts)
-- | Correct
unwrap4 :: BHeap a -> [Node a]
unwrap4 (BHeap x) = x

instance PriorityQueue BHeap where
  -- | Correct
  empty = BHeap []
  -- | Correct
  isEmpty (BHeap l) = null l
  -- | Correct
  insert x = ins4 Node { value=x, rank=0, children=[] }
  -- | Correct
  meld (BHeap []) ts = ts
  meld ts (BHeap []) = ts
  meld (BHeap (t1:ts1)) (BHeap (t2:ts2))
    | rank t1 < rank t2 = BHeap (t1:unwrap4 (meld (BHeap ts1) (BHeap (t2:ts2))))
    | rank t2 < rank t1 = BHeap (t2:unwrap4 (meld (BHeap (t1:ts1)) (BHeap ts2)))
    | otherwise = ins4 (link t1 t2) (meld (BHeap ts1) (BHeap ts2))
  -- | Correct
  findMin (BHeap []) = error "min of empty heap"
  findMin (BHeap [t]) = value t
  findMin (BHeap (t:ts)) = let x = findMin (BHeap ts)
                           in if value t <= x then value t else x
  -- | Wrong deleteMin.
  deleteMin (BHeap []) = error "delete min of empty heap"
  deleteMin (BHeap (t:ts)) = meld (BHeap (reverse . children $ t)) (BHeap ts)