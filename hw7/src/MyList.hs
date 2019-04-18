module MyList where
import MyListType
-- 1. Eq
instance Eq a => Eq (List a) where
    Nil == Nil         = True
    (x:~xs) == (y:~ys) = x == y && xs == ys
    _ == _             = False

-- 2. Ord
instance Ord a => Ord (List a) where
    (x:~xs) <= (y:~ys) = x < y || x <= y && xs <= ys
    Nil <= _           = True
    _ <= _             = False

-- 3. Show
show' (x:~Nil) = show x ++ "]"
show' (x:~xs) = show x ++ "," ++ show' xs
show' Nil = "]"
instance Show a => Show (List a) where
    show xs = "[" ++ show' xs
