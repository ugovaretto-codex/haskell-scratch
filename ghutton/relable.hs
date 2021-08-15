import System.Directory.Internal.Prelude (Monad)
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show t = show' t 0
              where 
                show' (Node l r) i = "\n|\n " ++ replicate (2*i) '-' ++ show' l (i+1) ++
                                     "\n|\n " ++ replicate (2*i) '-' ++ show' r (i+1)
                show' (Leaf x) _ = "- " ++ show x ++ "\n"

tree = Node 
          (Node
              (Leaf 'a')
              (Leaf 'b'))
          (Leaf 'c')

relabel' :: Tree a -> Int -> (Tree Int, Int)
relabel' (Leaf x) n = (Leaf n, n+1)
relabel' (Node l r) n = let (l', n') = relabel' l n
                            (r', n'') = relabel' r n'
                        in (Node l' r', n'')   

newtype ST st a = S {run :: st -> (a,st)}

instance Functor (ST a)  where
  fmap f st = S(\s -> let (x,s') = run st s
                      in (f x, s'))

instance Applicative (ST a) where
  pure x = S (\s -> (x,s))
  fs <*> st = S(\s -> let (f,s') = run fs s
                          (x,s'') = run st s'
                      in (f x, s''))  

instance Monad (ST a) where
  return x = S(\st -> (x,st))
  m >>= f = S(\s -> let (x',s') = run m s
                        (x'',s'') = run (f x') s'
                    in (x'',s''))

next :: ST Int Int
next = S (\n -> (n,n+1))


relabel :: (Tree a) -> ST Int (Tree Int)
relabel (Leaf x) = do
                    n <- next
                    pure (Leaf n)
relabel (Node l r) = do
                      l' <- relabel l
                      r' <- relabel r
                      pure (Node l' r')
label :: (Tree a) -> (Tree Int)
label t = fst . run (relabel t) $ 0  

main = do putStrLn "hello"

