module BHeap where

import PriorityQueue
import HeapNode

-- | Buggy Binomial Heap implementation 5
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
ins5 :: (Ord a) => Node a -> BHeap a -> BHeap a
ins5 t (BHeap []) = BHeap [t]
ins5 t (BHeap (tp:ts)) = if rank t < rank tp
                        then BHeap (t:tp:ts)
                        else ins5 (link t tp) (BHeap ts)

instance PriorityQueue BHeap where
  -- | Correct
  empty = BHeap []
  -- | Correct
  isEmpty (BHeap l) = null l
  -- | Correct
  insert x = ins5 Node { value=x, rank=0, children=[] }
  -- | Correct
  findMin (BHeap []) = error "min of empty heap"
  findMin (BHeap [t]) = value t
  findMin (BHeap (t:ts)) = let x = findMin (BHeap ts)
                           in if value t <= x then value t else x
  -- | Wrong due to wrong @meld@ -- Lee did not copy corrected @deleteMin@ from @BHeap.hs@ this @B5Heap.hs@
  deleteMin (BHeap []) = error "delete min of empty heap"
  deleteMin (BHeap (t:ts)) =
    let getMin t [] = (t, [])
        getMin t (tp:tsp) = let (tq, tsq) = getMin tp tsp
                            in if value t <= value tq then (t, ts) else (tq, t:tsq)
        (Node{ children=c }, tsq) = getMin t ts
    in meld (BHeap (reverse c)) (BHeap tsq)
  -- | Wrong meld
  -- No unwrap5 is used by wrong meld!
  meld (BHeap []) ts2 = ts2
  meld (BHeap (t1:ts1)) (BHeap ts2) =
    BHeap [Node { value=value t1, rank=rank t1, children=ts1 ++ ts2 }]