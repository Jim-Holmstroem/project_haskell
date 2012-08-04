
import Prime
import Listified


n=1000000
--MISS (old code): the problem where the consecutive primes has to start at 2 is too easy


main = print . continousSubSeq 
--main = print . reverse . (takeWhile (<n)) .(scanl1 (+)) $ primes
--main = print . (filter isPrime).(scanl1 (+)) $ primes
