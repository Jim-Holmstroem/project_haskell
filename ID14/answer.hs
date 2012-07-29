import Data.List

collatzUpdate:: Integer->Integer
collatzUpdate n
    | even n    = div n 2
    | otherwise = 3*n + 1

collatzTrivial = (++[1]) . (takeWhile (/=1)) . (flip (scanl (\x y->collatzUpdate x)) [1..])

--collatz:: [Integer]->[Integer] //didn't manage to make it work, could be faster by memozing already calculated paths from a certain state "last a" and just appending them
--collatz a@(_:_)
--    | 1<last a      = collatz (collatzUpdate(last a):a)
--    | otherwise     = a

main  = print .head .(maximumBy (\x y->compare (length x) (length y)) ).(map (collatzTrivial)) $ [1..1000000]

