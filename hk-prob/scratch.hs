import Data.Coerce
f :: (Num a, Fractional b) => [a] -> b
f  = map (\x -> coerce x)
