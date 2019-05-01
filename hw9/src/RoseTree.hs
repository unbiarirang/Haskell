module RoseTree where
import RoseTreeType

instance Functor RoseTree where -- 写出 fmap 的定义
    fmap f (Node x xs) = Node (f x) (map (fmap f) xs)
