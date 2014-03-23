
module Prime
( primeFactors
, primes
, isPrime
) where

primeFactors :: Int -> [Int]
primeFactors n = factor primes n
  where
    factor ps@(p:pt) n | p*p > n      = [n]
                       | rem n p == 0 = p : factor ps (quot n p)
                       | otherwise    =     factor pt n
    -- primes = 2 : filter (\n-> n==head(factor primes n)) [3,5..]
    -- primes = 2 : filter isPrime [3,5..]      -- isPrime of Q.31
    primes = primesTME

-- duplicates-removing union of two ordered increasing lists
union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys

{-# OPTIONS_GHC -O2 -fno-cse #-}
-- tree-merging Eratosthenes sieve
--  producing infinite list of all prime numbers
primesTME :: [Int]
primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t
                    | True  = k : gaps (k+2) xs

primes = primesTME

isPrime = (==1).(length).(primeFactors)
