module BinaryHeight where
import BinTree

height :: BinTree.BinTree a -> Integer
height Nil = 0
height (Node Nil Nil _) = 1
height (Node l r _) = max (height l) (height r) + 1
