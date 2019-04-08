module MaxSubstrSum where
solution :: [Integer] -> Integer
solution xs = (\(x,y) -> x) $ foldl func (0,0) xs

func :: (Integer, Integer) -> Integer -> (Integer, Integer)
func (max,acc) x
    | acc + x > max = (acc+x, acc+x)
    | acc + x > x = (max, acc+x)
    | otherwise = (max, x)
