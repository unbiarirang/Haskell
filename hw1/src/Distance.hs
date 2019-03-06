module Distance where

solution :: Integer -> (Double, Double) -> (Double, Double) -> Double
solution p (x1, x2) (y1, y2)
    | p == 1 = abs (x1 - y1) + abs (x2 - y2)
    | p == 2 = sqrt ((x1 - y1) ^ 2 + (x2 - y2) ^ 2)
    | otherwise = maximum [abs (x1 - y1), abs (x2 - y2)]
