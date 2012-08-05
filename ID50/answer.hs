
import Prime
import Listified
import Data.List

--MISS (old code): the problem where the consecutive primes has to start at 2 is too easy
--First version uses to much RAM (15G+), must generate the subsequence in order, cant make them out of order and then sort them.

--main = print.(take 1).(filter isPrime).(filter (<n)).(map sum).reverse.continousSubSeqs.(takeWhile (<(div n 16))) $ primes

--the assumption that n > 1000 which makes the sequence at least 21 (as in the example)
--assuming it will never have to stop (since it fails in the end)

n=10000

splitter = nub.concat.(map (\x->[init x,tail x]))

continousSubSeqsSorted a@(x:xs) = a : [splitter a]
--continousSubSeqsSorted a@(x:xs)= a:[(nub.concat.(map (\x->[init x,tail x])) $ a)]

availablePrimes = (takeWhile (<div n 11)) $ primes

--main = print.(take 1).(filter isPrime).(filter (<n)).(map sum).concat.continousSubSeqsSorted $ [availablePrimes] --the first one passing thru all the filters will be the largest (could be others with the same length)

main = print.(take 1).(filter isPrime).(filter (<n) ). (map sum) .concat. (scanl (\x y->(concat.continousSubSeqsSorted) x) [availablePrimes]) $ repeat [availablePrimes]--(1.. is just a dummy to kick it into gear) ugly to avoid, in this case, ugly recursion
--main = putStr.unlines.(map show).(take 10).concat. (scanl (\x y->(concat.continousSubSeqsSorted) x) [availablePrimes]) $ repeat [availablePrimes]--(1.. is just a dummy to kick it into gear) ugly to avoid, in this case, ugly recursion
--main = putStr . unlines . (map show) . continousSubSeqsSorted $ [availablePrimes]

