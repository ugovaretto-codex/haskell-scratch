-- | Main entry point to the application.
module Main where

import Control.Monad.Reader
import Control.Monad.Writer

{- 
    The ReaderT transformer is used to retrieve a read-only value from some environment. 
    The WriterT transformer will log the result of each retrieval. 
    Running the two transformers together yields a log of each step along with the actual results.
-}

data Person = Person { name :: String }
    deriving (Show)

alex    = Person "Alex Fontaine"
philip  = Person "Philip Carpenter"
kim     = Person "Kim Lynch"

peopleDb = [alex, philip, kim]

-- Get person from environment, log the person, and print name
process :: ReaderT Person (WriterT String IO) ()
process = do
    tell "Looking up a person. "
    Person p <- ask
    tell $ "Found person: " ++ p ++ ". "
    liftIO $ putStrLn p

-- Get person from environment, log the person, and return the name
process' :: ReaderT Person (WriterT String IO) String
process' = do
    tell "Looking up a person... "
    Person p <- ask
    tell $ "Found person: " ++ p ++ ". "
    return p
    
main :: IO ()
main = do
    -- Print name from monad transformer
    result1 <- runWriterT (runReaderT process alex) -- :: ((), String)
    putStrLn $ snd result1

    -- Extract the name from monad transfomer, then print it
    result2 <- runWriterT (runReaderT process' alex) -- :: (String, String)
    putStrLn $ fst result2
    putStrLn $ snd result2

    -- Now do the same thing for a list of people using mapM
    result3 <- runWriterT (mapM (runReaderT process') peopleDb) -- :: ([String], String)

    let people = fst result3
        log    = snd result3

    putStrLn "\n\nReaderT values:\n"
    mapM_ putStrLn people
    
    putStrLn "\nWriterT log:\n"
    putStrLn $ log
    
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