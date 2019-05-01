module BHeap where

import PriorityQueue
import HeapNode

-- | Buggy Binomial Heap implementation 3
newtype BHeap a = BHeap [Node a] deriving (Show)

-- | Wrong link. Then @ins@ and @meld@ go wrong because they directly call @link@. And indirectly, @insert@ and @deleteMin@ go wrong.
link3 t1 t2 = if value t1 <= value t2
  then Node { value=value t1,
              rank=rank t1 + 1,
              children=t1:children t1 }
  else Node { value=value t2,
              rank=rank t2 + 1,
              children=t2:children t2 }

-- | Wrong due to wrong @link@
ins3 :: (Ord a) => Node a -> BHeap a -> BHeap a
ins3 t (BHeap []) = BHeap [t]
ins3 t (BHeap (tp:ts)) = if rank t < rank tp
                        then BHeap (t:tp:ts)
                        else ins3 (link3 t tp) (BHeap ts)
  
-- | Correct
unwrap3 :: BHeap a -> [Node a]
unwrap3 (BHeap x) = x

instance PriorityQueue BHeap where
  -- | Correct
  empty = BHeap []
  -- | Correct
  isEmpty (BHeap l) = null l
  -- | Wrong due to wrong @ins@
  insert x = ins3 Node { value=x, rank=0, children=[] }
  -- | Wrong due to wrong @link@ and @ins@
  meld (BHeap []) ts = ts
  meld ts (BHeap []) = ts
  meld (BHeap (t1:ts1)) (BHeap (t2:ts2))
    | rank t1 < rank t2 = BHeap (t1:unwrap3 (meld (BHeap ts1) (BHeap (t2:ts2))))
    | rank t2 < rank t1 = BHeap (t2:unwrap3 (meld (BHeap (t1:ts1)) (BHeap ts2)))
    | otherwise = ins3 (link3 t1 t2) (meld (BHeap ts1) (BHeap ts2))
  -- | Correct
  findMin (BHeap []) = error "min of empty heap"
  findMin (BHeap [t]) = value t
  findMin (BHeap (t:ts)) = let x = findMin (BHeap ts)
                            in if value t <= x then value t else x
  -- | Wrong due to wrong @meld@
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