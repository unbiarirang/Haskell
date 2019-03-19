module MyList where

import Prelude hiding (concat)

data List a = a :~ (List a) | Nil
              deriving (Show)
infixr 5 :~

concat :: List (List a) -> List a
concat Nil           = Nil
concat (x1:~x2:~Nil) = concat' x1 x2
concat (x:~xs)       = concat' x (concat xs)

concat' :: List a -> List a -> List a
concat' x Nil       = x
concat' Nil x       = x
concat' (xs:~Nil) y = xs :~ y
concat' (x:~xs) y   = x :~ (concat' xs y)
