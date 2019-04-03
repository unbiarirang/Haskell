module MaxSubstrSum where
solution :: [Integer] -> Integer
solution xs = (\(x,y,z) -> x) $ foldl func (0,0,0) xs

func :: (Integer, Integer, Integer) -> Integer -> (Integer, Integer, Integer)
func (0,0,0) x = (0,x,0)
func (acc, prev, flag) x
    | flag == -2 = (acc, 0, -2)
    | prev == x + 1 && flag == -1 = (acc+x, x, flag)
    | prev == x - 1 && flag == 1 = (acc+x, x, flag)
    | prev == x + 1 && flag == 0 = (prev+x, x, -1)
    | prev == x - 1 && flag == 0 = (prev+x, x, 1)
    | flag == 0 = (acc, x, flag)
    | otherwise = (acc, 0, -2)
