-- mergeOrd :: (Ord a) => [a] -> [a] -> [a]
-- mergeOrd xs [] = xs
-- mergeOrd [] ys = ys
-- mergeOrd (x:xs) (y:ys) = let xs' = if x <= y then xs else x:xs
--                              ys' = if x > y then ys else y:ys
--                              v = if x <= y then x else y
--                          in
--                            v : mergeOrd xs' ys'
-- mergeSort :: (Ord a) => [a] -> [a]
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort xs = let l = length xs `div` 2
--                in
--                 mergeOrd (mergeSort (take l xs)) (mergeSort (drop l xs))


-- dropElems :: Int -> [a] -> [a]
-- dropElems _ [] = []
-- dropElems 0 xs = xs
-- dropElems n (x:xs) = dropElems (n-1) xs

-- dropLast :: [a] -> [a]
-- dropLast [] = []
-- dropLast [x] = []
-- dropLast (x:xs) = x : dropLast xs

-- pos :: (Num a, Ord a) => a -> Maybe a
-- pos x
--     | x < 0 = Nothing 
--     | otherwise = Just x

-- data Nat= Zero | Succ Nat deriving Show 
-- int2nat :: Int -> Nat
-- int2nat 0 = Zero
-- int2nat n = Succ $ int2nat (n-1)

-- nat2int :: Nat -> Int 
-- nat2int Zero = 0
-- nat2int (Succ m) = 1 + nat2int m 

-- addNat :: Nat -> Nat -> Nat
-- addNat Zero n = n
-- addNat (Succ m) n = Succ (addNat m n) 

-- data Expr = Value Int
--           | Add Expr Expr
--           | Mul Expr Expr deriving Show

-- printExpr :: Expr -> IO ()
-- printExpr (Value n) = do
--                         putStr $ show n
--                         putStr " "
-- printExpr (Add e1 e2) = do
--                             printExpr e1
--                             putStr " + "
--                             printExpr e2
--                             putStr " "
-- printExpr (Mul e1 e2) = do
--                             printExpr e1
--                             putStr " * "
--                             printExpr e2
--                             putStr " "

-- eval :: Expr -> Int 
-- eval (Value n) = n
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- folde :: (Int -> Int) -> (Int -> Int -> Int) 
--          -> (Int -> Int -> Int) -> Expr -> Int
-- folde v _ _ (Value n) = v n
-- folde v a m (Add e1 e2) = folde v a m e1 `a` folde v a m e1
-- folde v a m (Mul e1 e2) = folde v a m e1 `m` folde v a m e1  

-- iid :: Int -> Int 
-- iid x = x

-- mul :: Nat -> Nat -> Nat
-- mul (Succ Zero) n = n
-- mul m (Succ Zero) = m
-- mul m (Succ n) = addNat m (mul m n)

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- type TreeInt = Tree Int

-- insert :: [a] -> a -> [a] -> [[a]]
-- insert hs n [] = [hs ++ [n]] 
-- insert hs n (x:xs) = (hs ++ (x : n : xs)) : insert (hs ++ [x]) n xs

-- group :: [a] -> [[a]]
-- group [] = [[]]
-- group [x] = [[x]]
-- group (x:xs) = (x:xs) : group xs


-- combine :: Eq a => [a] -> [[a]] -> [[a]]
-- combine [] xs = xs
-- combine xs [] = [xs]
-- combine xs ys = [x:y | x <- xs, y <- ys, x `notElem` y]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
ins :: a -> [a] -> [[a]]
ins n xs = insert' [] n xs where
    insert' hs n [] = [hs ++ [n]] 
    insert' hs n (x:xs) = (hs ++ (n : x : xs)) : insert' (hs ++ [x]) n xs
    
perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concat [ins x ys | ys <- perm xs]

subseqs :: Int -> [a] -> [[a]]
subseqs 0 _ = [[]]
subseqs _ [] = []
subseqs n (x:xs) = map (x:) (subseqs (n-1) xs) ++ subseqs n xs

choices :: [a] -> [[a]]
choices [] = []
choices xs = concat [perm ys | i <- [1..], ys <- subseqs i xs]

split :: [a] -> [([a], [a])]
split = split' [] where
    split' _ [] = []
    split' hs [x] = []                                                                                                                                  
    split' hs (x':xs') = (hs ++ [x'], xs') : split' (hs ++ [x']) xs' 


--------------------------------------------------------------------------------

data Op = Add | Sub | Mul | Div deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool 
valid Add _ _ = True 
valid Sub x y = x > y
valid Mul _ _ = True 
valid Div x y = x `mod` y  == 0

data Expr = Val Int | App Op Expr Expr 
            deriving Show

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r)  = [apply o x y | x <- eval l,
                                   y <- eval r,
                                   valid o x y]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
                  && eval e == [n]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine e1 e2 = [App op e1 e2 | op <- [Add, Mul, Sub, Div]] 

solutions :: [Int] -> Int -> [Expr]
solutions [] _ = []
solutions [n] s = [Val x | n == s, x <- [n]]
solutions xs n = [ e | xs' <- choices xs
                     , e <- exprs xs'
                     , eval e == [n]]
--------------------------------------------------------------------------------
valid' :: Op -> Int -> Int -> Bool 
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1 
valid' Div x y = x `mod` y  == 0 && y /= 1

type Result = (Expr, Int)

solutions':: [Int] -> Int -> [Expr]
solutions' xs n = [ e | xs' <- choices xs
                     , (e,m) <- results xs'
                     , m == n]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns
              , l       <- results ls
              , r       <- results rs
              , res     <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (e1, n1) (e2, n2) = 
    [(App op e1 e2, apply op n1 n2) | op <- [Add, Mul, Sub, Div]
                                    , valid' op n1 n2] 

main =
    do
        let sol = solutions' [1,3,7,10,25,50] 765
        print $ take 1 sol