newtype State s a = State { runState :: s -> (a, s)}

instance Show (State s a) where
  show s = show "state"

-- look at our counter and return "foo" or "bar"  
-- along with the incremented counter:  
fromStoAandS :: Int -> (String,Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
               | otherwise = ("bar",c+1)

stateIntString :: State Int String
stateIntString = State {runState = fromStoAandS}
-- tick :: State -> ((), State)
-- tick = \s -> ((), s+1)

instance Functor (State s) where
  fmap f (State g) = State(\s -> let (x, s') = g s in
                                 (f x, s'))

-- instance Applicative  (State s) where
--   pure x = State $ \s -> (x,s)
--   State f <*> State s = State $ do
--     (fa, fs) <- f
--     let (sa, ss) = s fs
--     return (fa sa, ss)
instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  -- <*> :: State s (a->b) -> State s a -> State s b
  (<*>) (State sf) (State sa) =
    State (\s -> let (fn, fs) = sf s
                     (a, s') = sa fs
                 in (fn a, s'))
                  

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (>>=) m k = State $ \s -> let (a, s') = runState m s
                            in runState (k a) s'


ff :: String -> State Int String
ff str = State (\s -> (str ++ " ...", s + 2))

data Person = Person {firstName :: String, lastName :: String} deriving Show

-- >let f = State {runState = fromStoAandS} >>= ff >>= ff
-- >runState f 1