
import Prime

n=1000000
main = print . head . (filter isPrime) . reverse . (takeWhile (<n)) . (scanl1 (+)) $ primes

