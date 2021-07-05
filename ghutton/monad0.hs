pairs' :: [a] -> [b] -> [(a,b)]
pairs' xs ys = xs >>= (\x -> ys >>= 
                      (\y -> [(x,y)]))

