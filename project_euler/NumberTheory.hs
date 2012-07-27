
module NumberTheory
( listgcd
, listlcm
) where


-- gcd on a list

listgcd = foldl1 (gcd)

-- lcm on a list

listlcm = foldl1 (lcm) 

