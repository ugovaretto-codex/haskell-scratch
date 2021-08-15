import Control.Monad
import Control.Applicative
import GHC.Conc (runSparks)

newtype R env a = R {runR :: env ->  a}

instance Functor (R env) where
  fmap f (R e) = R(f . e)

instance Applicative (R env) where
  pure x = R (const x)
  R f <*> R ea = R (\e -> (f e) (ea e))

instance Monad (R env) where
  return  = pure
  rx >>= f = R(\e -> let a = runR rx e
                     in runR (f a) e)

ask :: R env env
ask = R(id)

asks :: (e -> a) -> R e a
asks f = ask >>= (pure . f) 

newtype W w m = W{runW :: (m,w)}

instance Functor (W w) where
  fmap f (W (x,w)) = W((f x, w))

instance (Monoid w) => Applicative (W w) where
  pure x = W(x,mempty)
  (W(f,w)) <*> (W(x,w')) = W(f x, w `mappend` w')

instance (Monoid w) => Monad (W w) where
  return  = pure
  (W (x,w)) >>= f = let W((x',w')) = f x
                    in W((x',w `mappend` w'))

tell :: w -> W w ()
tell w' = W (((),w'))

newtype ST st a = S {runS :: st -> (a,st)}

instance Functor (ST a)  where
  fmap f st = S(\s -> let (x,s') = runS st s
                      in (f x, s'))

instance Applicative (ST a) where
  pure x = S (\s -> (x,s))
  fs <*> st = S(\s -> let (f,s') = runS fs s
                          (x,s'') = runS st s'
                      in (f x, s''))  

instance Monad (ST a) where
  return x = S(\st -> (x,st))
  m >>= f = S(\s -> let (x',s') = runS m s
                        (x'',s'') = runS (f x') s'
                    in (x'',s''))

-- newtype State s a = State { runState :: s -> (a, s) }

-- Structure: StateT function > Monad > Tuple
-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- instance (Functor m) => Functor (StateT s m) where
--   fmap f (StateT a) = StateT $ \s -> first f <$> a s

-- instance (Monad m) => Applicative (StateT s m) where
--   pure a = StateT $ \s -> pure (a, s)
--   StateT g <*> StateT h = StateT $ \s -> do
--     (f, s') <- g s
--     (x, s'') <- h s'
--     return (f x, s'')

-- instance (Monad m) => Monad (StateT s m) where
--   return = pure
--   StateT sma >>= f = StateT $ \s -> sma s >>= \(a, s') -> runStateT (f a) s'

----
pop :: ST [Int] Int
pop = S(\(x:xs) -> (x,xs))
push :: Int -> ST [Int] Int
push n = S(\xs -> (n,n:xs))
get :: ST s s
get = S(\s -> (s,s))
put :: s -> ST s ()
put s = S(\_ -> ((), s))



main = do putStrLn "ciao"