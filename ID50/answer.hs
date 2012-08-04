
import Prime
import Listified
import Data.List


n=1000000
--MISS (old code): the problem where the consecutive primes has to start at 2 is too easy

-- <div n 2 is under the assumption that it exists some sequence bigger then 2 (which is true for n larger then 

main = print . (take 1) . (filter isPrime) . (filter (<n)) . (map sum) . reverse . (sortBy (\x y->(length x) `compare` (length y))) . continousSubSeqs . (takeWhile (<(div n 2))) $ primes

