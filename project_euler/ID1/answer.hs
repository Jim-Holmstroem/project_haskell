
is_divisor a = any (==0) $ map ($ a) (map (flip (mod)) [3,5])
divisors = filter is_divisor [1..]
main = print $ sum $ takeWhile (<1000) $ divisors

