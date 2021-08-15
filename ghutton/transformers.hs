module Main where

import Data.Char (isUpper)
import Control.Monad.Trans.State

---- IOMaybe
type IOMaybe a = IO (Maybe a)

returnIOM :: a -> IOMaybe a
returnIOM = return . Just

bindIOM :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
bindIOM iom f = do
  maybe_val <- iom
  case maybe_val of
    Nothing -> return Nothing
    (Just v) -> f v

(>>>=) :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
(>>>=) = bindIOM

liftIO :: IO a -> IOMaybe a
liftIO io = io >>= returnIOM

checkInput :: String -> Bool
checkInput [] = False
checkInput (x : _) = isUpper x

getName :: IOMaybe String
getName = do
  input <- getLine
  if checkInput input
    then returnIOM input
    else return Nothing

example :: IOMaybe ()
example =
  putStr "Please enter your name: "
    >> getName
    >>>= (\s -> liftIO (putStrLn ("Your name is: " ++ s)))

--- MaybeT
-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
-- instance (Monad m) => Monad (MaybeT m) where
--   return  = MaybeT . return . Just
--   x >>= f = MaybeT(do
--     v <- runMaybeT x
--     case v of
--       Nothing -> return Nothing
--       Just y -> runMaybeT (f y)
--     )


main :: IOMaybe ()
main = example