module BinaryBalance where
import BinTree
import BinaryHeight(height)

isBalance :: BinTree.BinTree a -> Bool
isBalance Nil = True
isBalance (Node Nil Nil _) = True 
isBalance (Node l r _) = (abs (height l - height r) < 2)
                         && isBalance l
                         && isBalance r
