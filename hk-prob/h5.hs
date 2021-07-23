
module Main where
import Text.Printf ()
import qualified Data.Sequence as S
sortedMedian :: (Real a, Fractional b) => S.Seq a -> b
sortedMedian xs = let 
                    (_ S.:|> m1, m2 S.:<| _) = S.splitAt (S.length xs `div` 2) xs
                    m | even . length $ xs = realToFrac (m1 + m2) * 0.5
                      | otherwise = realToFrac m2 -- length of second half > length of first half 
            in m

quart :: (Real a, Fractional b) => S.Seq  a -> [b]
quart xs = let xs' = S.sort xs
               (ls,r S.:<| rs) = S.splitAt (length xs' `div` 2) xs'
               rs' | even (length xs') = r S.<| rs
                  | otherwise = rs
               in [sortedMedian ls, sortedMedian xs', sortedMedian rs']

expand :: S.Seq a -> S.Seq Int -> S.Seq a
expand es fs = foldr (S.><) S.empty $ (\(v,c) -> S.replicate c v) <$> S.zip es fs

quarts :: (Real a, Fractional b) => (S.Seq a, S.Seq Int) -> b
quarts (es,fs) = let [q1,_,q3] = quart $ expand es fs
                 in (q3 - q1)


parseInput :: String -> (S.Seq Int, S.Seq Int)
parseInput s = let (n S.:<| rs) = fmap (read :: String->Int) . S.fromList . words $ s
               in (S.take n rs, S.drop n rs)

main = interact  $ show . quarts . parseInput