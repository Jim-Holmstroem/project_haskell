
import Prime
import Listified
import Data.List


n=100
--MISS (old code): the problem where the consecutive primes has to start at 2 is too easy
--First version uses to much RAM (15G+), must generate the subsequence in order, cant make them out of order and then sort them.

--main = print.(take 1).(filter isPrime).(filter (<n)).(map sum).reverse.continousSubSeqs.(takeWhile (<(div n 16))) $ primes

--the assumption that n > 1000 which makes the sequence at least 21 (as in the example)
availablePrimes = (takeWhile < div n 20) $ primes

main = print.(take 1).(filter isPrime).(filter (<n)).(map sum).
