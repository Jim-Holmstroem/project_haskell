
module NumberTheory
( listgcd
, listlcm
, nCr
) where

-- gcd on a list

listgcd = foldl1 (gcd)

-- lcm on a list

listlcm = foldl1 (lcm) 

nCr::Integer->Integer->Integer
nCr n r = div (product [(n-r+1)..n]) (product [2..r]) 


