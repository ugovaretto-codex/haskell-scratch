newtype State s a = 
  State {
    runState ::
      s -> (a,s) 
  }

-- State Account Bool

instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f sa = 
    let x = runState sa
    



