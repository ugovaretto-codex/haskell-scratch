module Main where
import qualified Data.Map as M hiding (foldr)
import qualified Data.Sequence as S
inc :: (Ord a, Num b, Eq a, Eq b) => a -> b -> M.Map a b -> M.Map a b
inc k i = M.alter (\x -> if x == Nothing then Just i else (+1) <$> x) k
hist :: Ord a => [a] -> M.Map a Int
hist = foldr (\k m -> inc k 1 m)  M.empty

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / realToFrac (length xs)

median :: (Ord a, Real a, Fractional b) => [a] -> b
median xs = let xs' = S.sort . S.fromList $ xs
                (_ S.:|> m1, m2 S.:<| _) = S.splitAt (S.length xs' `div` 2) xs'
                m | even . length $ xs' = realToFrac (m1 + m2) * 0.5
                  | otherwise = realToFrac m2 -- assume length of second half > length of first half 
            in m
mode :: (Num a, Ord a) => [a] -> a
mode xs = let (k,_) = M.foldlWithKey 
                        (\(mk,mv) k v -> 
                            if v > mv 
                            then (k,v) 
                            else (mk,mv)) (0,0) $ hist xs
          in k

mmm :: (Show a, Real a) => [a] -> [String]
mmm xs = [show $ mean xs, show $ median xs, show $ mode xs]

main :: IO ()
main = interact $ unlines . mmm . map (read :: String->Int) . tail . words