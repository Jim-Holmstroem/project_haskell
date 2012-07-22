fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
even' = ((==0).(flip (mod) 2)) --oops, exists builtin even
main = print $ sum $ filter even $ takeWhile (4000000>=) fibs
