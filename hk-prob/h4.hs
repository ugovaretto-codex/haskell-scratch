module Main where
import Text.Printf
import qualified Data.Sequence as S
sortedMedian :: (Real a, Fractional b) => S.Seq a -> b
sortedMedian xs = let 
                    (_ S.:|> m1, m2 S.:<| _) = S.splitAt (S.length xs `div` 2) xs
                    m | even . length $ xs = realToFrac (m1 + m2) * 0.5
                      | otherwise = realToFrac m2 -- length of second half > length of first half 
            in m

quart :: Real a => S.Seq  a -> [Float]
quart xs = let xs' = S.sort xs
               (ls,r S.:<| rs) = S.splitAt (length xs' `div` 2) xs'
               rs' | even (length xs') = r S.<| rs
                  | otherwise = rs
               in [sortedMedian ls, sortedMedian xs', sortedMedian rs']

main :: IO ()
main = interact $ unlines . map (printf "%.0f") . quart . S.fromList . map (read :: String->Int) . tail . words

