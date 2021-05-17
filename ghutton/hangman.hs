module Hangman where
import System.IO
hangman :: IO ()
hangman = do putStrLn "Think of a word"
             word <- sgetline
             putStrLn "Try to guess it:"
             play word

sgetline :: IO String 
sgetline = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '_'
                   xs <- sgetline
                   return (x:xs)

getCh :: IO Char 
getCh = do hSetEcho stdin False
           c <- getChar 
           hSetEcho stdin True
           return c

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine 
               if guess == word then
                 putStrLn "You got it!!"
               else
                 do putStrLn (match '-' word $ align ' ' word guess)
                    play word

main = hangman 

-- match :: String -> String -> String
-- match xs ys = [if elem x ys then x else '-' | x <- xs]
match :: Char -> String -> String -> String 
match s = zipWith (\x y -> if x == y then x else s)

align :: a -> [a] -> [a] -> [a]
align x xs ys | length ys < length xs = pad x (length xs - length ys) ys
              | otherwise = ys

pad :: a -> Int -> [a] -> [a]
pad _ 0 xs = xs
pad x n xs = xs ++ replicate n x
