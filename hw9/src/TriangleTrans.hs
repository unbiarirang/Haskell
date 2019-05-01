module TriangleTrans where

import MyLens
import TriangleType

coordXLens f (Point x y) = fmap (\ x' -> Point x' y) (f x)
coordYLens f (Point x y) = fmap (\ y' -> Point x y') (f y)
triALens f (Triangle a b c) = fmap (\ a' -> Triangle a' b c) (f a)
triBLens f (Triangle a b c) = fmap (\ b' -> Triangle a b' c) (f b)
triCLens f (Triangle a b c) = fmap (\ c' -> Triangle a b c') (f c)
