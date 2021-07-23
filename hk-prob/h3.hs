module Main where

import Text.Printf (printf)

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / realToFrac (length xs)

stdev :: Real a => [a] -> Float
stdev xs =
  let m = mean xs
      v =
        (sum . map ((\x -> (x - m) * (x - m)) . realToFrac) $ xs)
          / realToFrac (length xs)
   in sqrt v

main :: IO ()
main = interact $ printf "%.1f" . stdev . map (read :: String -> Int) . tail . words