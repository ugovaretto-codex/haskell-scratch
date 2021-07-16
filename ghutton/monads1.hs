{-# LANGUAGE TupleSections #-}
data Expr = Val Int | Div Expr Expr

-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n -> case eval y of
--                         Nothing -> Nothing 
--                         Just m -> safediv n m

-- eval :: Expr -> Maybe Int
-- eval (Val n) = return n
-- eval (Div x y) =  eval x >>= (\n -> 
--                     eval y >>= (\m ->
--                         safediv n m))
eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = do
                    n <- eval x
                    m <- eval y
                    safediv n m

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)


type State  = Int 
type Guard = Bool 

newtype ST a = S (State -> (a,State))
app :: ST a -> State -> (a, State)
app (S st) s = st s

inc4 :: State -> (Guard, State)
inc4 s | s == 4 = (True, s)
       | otherwise = (False, s+1)



incST :: Guard -> ST Guard
incST b = S inc4

-- make it into a functor (fmap) and applicative (pure, <*>)

instance Monad ST where
    -- return :: a -> ST a
    return x = S (\s -> (x,s))
    -- >>= :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s
                        in app (f x) s' )
