primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [ x | x<-xs, mod x p > 0 ]
main = print $ primes !! (10001-1)
