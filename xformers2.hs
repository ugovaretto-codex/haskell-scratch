-- | Main entry point to the application.
module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(lift))
import DynFlags (xopt_set_unlessExplSpec)
{- 
    The ReaderT transformer is used to retrieve a read-only value from some environment. 
    The WriterT transformer will log the result of each retrieval. 
    Running the two transformers together yields a log of each step along with the actual results.
-}
newtype Person = Person { name :: String } deriving (Show)

alex :: Person
alex    = Person "Alex Fontaine"
philip :: Person
philip  = Person "Philip Carpenter"
kim :: Person
kim     = Person "Kim Lynch"

peopleDb = [alex, philip, kim]

-- logM :: String -> ReaderT Person (WriterT String IO) ()
logM :: Monad m => w -> ReaderT r (WriterT w m) ()
logM msg = ReaderT (\_ -> writer ((), msg))

logM' ::  w -> ReaderP r (WriterP w ())
logM' msg = reader' (\_ -> WriterP ((), msg))

newtype WriterP w m = WriterP{runW :: (m,w)}
newtype ReaderP env v = ReaderP{runR :: env -> v}

writer' :: w -> m -> WriterP w m
writer' w m = WriterP (m,w)

reader' :: (env -> v) -> ReaderP env v
reader' = ReaderP

ask' :: ReaderP env env
ask' = ReaderP id


toNull :: a -> ()
toNull = const ()
-- Get person from environment, log the person, and print name
process :: ReaderT Person (WriterT String IO) ()
process = do
    -- lift . tell $ "Looking up a person.
    logM "Looking up a person."
    Person p <- ask
    --lift . tell $ "Found person: " ++ p ++ ". "
    logM $ "Found person: " ++ p ++ ". "
    -- liftIO $ putStrLn p
    ReaderT (\_ -> writer (toNull(putStrLn p),""))

-- -- Get person from environment, log the person, and print name
-- process'' :: ReaderP Person (WriterP String ())
-- process'' = do
--     -- lift . tell $ "Looking up a person.
--     logM' "Looking up a person."
--     Person p <- ask'
--     --lift . tell $ "Found person: " ++ p ++ ". "
--     logM' $ "Found person: " ++ p ++ ". "
--     -- liftIO $ putStrLn p
--     ReaderP (\_ -> WriterP (toNull(putStrLn p), ""))

-- Get person from environment, log the person, and return the name
process' :: ReaderT Person (WriterT String IO) String
process' = do
    lift . tell $ "Looking up a person... "
    Person p <- ask
    lift . tell $ "Found person: " ++ p ++ ". "
    return p
    
main :: IO ()
main = do
    -- Print name from monad transformer
    result1 <- runWriterT (runReaderT process alex) -- :: ((), String)
    putStrLn $ snd result1

    -- Extract the name from monad transfomer, then print it
    (f,s) <- runWriterT (runReaderT process' alex) -- :: (String, String)
    putStrLn f
    putStrLn s

    -- Now do the same thing for a list of people using mapM
    result3 <- runWriterT (mapM (runReaderT process') peopleDb) -- :: ([String], String)

    let people = fst result3
        log    = snd result3

    putStrLn "\n\nReaderT values:\n"
    mapM_ putStrLn people
    
    putStrLn "\nWriterT log:\n"
    putStrLn log
    
    return ()

---OUTPUT---
{-

Alex Fontaine
Looking up a person. Found person: Alex Fontaine. 
Alex Fontaine
Looking up a person... Found person: Alex Fontaine. 


ReaderT values:

Alex Fontaine
Philip Carpenter
Kim Lynch

WriterT log:

Looking up a person... Found person: Alex Fontaine. Looking up a person... Found person: Philip Carpenter. Looking up a person... Found person: Kim Lynch. 
-}