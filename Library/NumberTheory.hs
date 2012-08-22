
module NumberTheory
( listgcd
, listlcm
, nCr
, properDivisors
, amicable
, isPerfectNumber
, isDeficientNumber
, isAbundantNumber
) where

-- gcd on a list

listgcd = foldl1 (gcd)

-- lcm on a list

listlcm = foldl1 (lcm) 

nCr::Integer->Integer->Integer
nCr n r = div (product [(n-r+1)..n]) (product [2..r]) 

properDivisors::Integer->[Integer]
properDivisors n = filter ((==0).(n `mod`)) $ [1..(div n 2)]

d = sum . properDivisors

amicable = (filter (\a->(d.d$a)==a)).(filter (\a->(d a)/=a))

isPerfectNumber::Integer->Bool
isPerfectNumber = \n->(d n)==n
isDeficientNumber::Integer->Bool
isDeficientNumber = \n->(d n)<n
isAbundantNumber::Integer->Bool
isAbundantNumber = \n->(d n)>n

