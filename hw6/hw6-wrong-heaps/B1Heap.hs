module BHeap where

import PriorityQueue
import HeapNode

-- | Buggy Binomial Heap implementation 1
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
ins1 :: (Ord a) => Node a -> BHeap a -> BHeap a
ins1 t (BHeap []) = BHeap [t]
ins1 t (BHeap (tp:ts)) = if rank t < rank tp
                        then BHeap (t:tp:ts)
                        else ins1 (link t tp) (BHeap ts)
-- | Correct
unwrap1 :: BHeap a -> [Node a]
unwrap1 (BHeap x) = x

instance PriorityQueue BHeap where
  -- | Correct
  empty = BHeap []
  -- | Correct
  isEmpty (BHeap l) = null l
  -- | Correct
  insert x = ins1 Node { value=x, rank=0, children=[] }
  -- | Correct
  meld (BHeap []) ts = ts
  meld ts (BHeap []) = ts
  meld (BHeap (t1:ts1)) (BHeap (t2:ts2))
    | rank t1 < rank t2 = BHeap (t1:unwrap1 (meld (BHeap ts1) (BHeap (t2:ts2))))
    | rank t2 < rank t1 = BHeap (t2:unwrap1 (meld (BHeap (t1:ts1)) (BHeap ts2)))
    | otherwise = ins1 (link t1 t2) (meld (BHeap ts1) (BHeap ts2))
  -- | Wrong findMin
  findMin (BHeap []) = error "min of empty heap"
  findMin (BHeap (t:ts)) = value t
  -- | Correct
  deleteMin (BHeap []) = error "delete min of empty heap"
  deleteMin (BHeap (t:ts)) =
    let getMin t0 t0s = case t0s of
          [] -> (t0, [])
          tp:tsp -> let (tq, tsq0) = getMin tp tsp
                    in if value t0 <= value tq
                       then (t0, t0s)
                       else (tq, t0:tsq0)
        (Node{ children=c }, tsq) = getMin t ts
    in meld (BHeap (reverse c)) (BHeap tsq)