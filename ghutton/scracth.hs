match :: Char -> String -> String -> String 
match s = zipWith (\x y -> if x == y then x else s)

pad :: a -> Int -> [a] -> [a]
pad _ 0 xs = xs
pad x n xs = xs ++ replicate n x

align :: a -> [a] -> [a] -> [a]
align x xs ys | length ys < length xs = pad x (length xs - length ys) ys
              | otherwise = ys