primes' : Int -> Lazy (List (Int))
primes' n = 
  primes'' [2..n] where
  primes'' : List (Int) -> List (Int)
  primes'' [] = []
  primes'' (x::xs) = x :: primes'' [y | y <- xs, y `mod` x /= 0]