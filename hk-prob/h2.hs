module Main where
import Text.Printf ( printf )
wmean :: (Real a, Fractional b) => [a] -> [a] -> b
wmean xs ws = realToFrac (sum (zipWith (*) xs ws)) / realToFrac (sum ws)

parseInput :: String -> ([Int], [Int])
parseInput s = let (v:rest) = map (read :: String -> Int) . words $ s
                   arr = take v rest
                   ws = drop v rest 
               in (arr, ws)

wm :: ([Int],[Int]) -> Float
wm (xs,ws) = wmean xs ws
main :: IO ()
main = interact $ (printf "%.1f") . wm . parseInput