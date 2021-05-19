primes :: [Int]
primes = [n | n <- [2..], and [n `mod` x /= 0 | x <- [2..n-1]]]

primes' :: [Int]
primes' = primes'' [2..] where
          primes'' (x:xs) = x : primes'' [y | y <- xs, y `mod` x /= 0]

diffPrimes :: [Int]
diffPrimes = zipWith (-) (tail primes') primes'

twinPrimes :: [(Int, Int)]
twinPrimes = [(p,q) | (p,q) <- zip primes' (tail primes'), q - p == 2]

main = do
        let xs = take 20000 primes' --primes' ~ 4x faster
        print (xs !! 19999)