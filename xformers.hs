import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans

newtype Counter = MkCounter {cValue :: Int}
  deriving (Show)

-- | 'inc c n' increments the counter by 'n' units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

-- | CounterS is a monad.
type CounterS = State Counter

-- | Increment the counter by 'n' units.
incS :: Int-> CounterS ()
incS n = state (\c-> ((),MkCounter(n + cValue c))) --modify (`inc` n)


-- | The computation we want to run, with the state monad.
mComputationS :: CounterS ()
mComputationS = do
  incS 3
  incS 3
  incS 3
  incS 5
  incS 5

type CounterRS = ReaderT Int CounterS
-- | Increment the counter by the amount of units specified by the environment.
incR :: CounterRS ()
incR = do
        x <- ask -- Int(runReader (reader (\i -> incS i)) x)
        ReaderT (\_ -> incS x)
        --x <- ask -- Int
        -- (lift . incS) x  -- (Int -> CounterS) -- ask >>= lift . incS

-- runState (runReaderT incR 5) (MkCounter 1)
-- runState (runReader (reader (\s -> state (\c -> ((),MkCounter (cValue c + s))))) 1) (MkCounter 10)