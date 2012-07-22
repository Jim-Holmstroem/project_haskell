divisors :: Integer -> [Integer]
divisors x = filter (\m->mod x m == 0) $ takeWhile ((<=x).(2*)) [2..] --howto do this without lambda?
main = print $ head $ filter ((>500).(+2).length.divisors) $ scanl1 (+) [1..] -- +2 comes from the missing nonproper/nontrivial divisors
