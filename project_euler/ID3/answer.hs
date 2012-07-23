primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [ x | x<-xs, mod x p > 0 ]

divides n p = mod n p == 0 


primeFactor :: Integer -> [Integer]
-- trivial
primeFactor n = filter (divides n) $ takeWhile ((<=n).(^2)) primes

firstPrimeFactor n from =  head $ filter (divides n) $ takeWhile ((<=n).(^2)) $ filter (>=from) primes

primeFactors n from = p : primeFactors $ div n p where p = firstPrimeFactor n from

main = print $ primeFactors 600851475143 2
-- main = print $ foldl1 max $ primeFactor 354321

