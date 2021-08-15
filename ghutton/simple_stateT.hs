module Main where
import Control.Monad ( void )
import Control.Monad.Trans.State ( StateT(runStateT), get, put )
import Control.Monad.IO.Class
main :: IO ()
main = void $ runStateT code [1..]

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    z <- push 10
    io $ print z
    return ()

pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x
    
push :: Integer -> StateT [Integer] IO Integer
push n  = do
  s <- get
  put (n:s)
  return n


io :: IO a -> StateT [Integer] IO a
io = liftIO
