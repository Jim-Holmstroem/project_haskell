import Debug.Trace
import Data.Int

innerTrace :: (Show a) => [Char] -> a -> a
--innerTrace message a = a 
innerTrace message a = trace (message ++ ":" ++ show a) a 

pref :: Int64 -> Int64
pref 0 = 1
pref 1 = 1
pref 2 = 2
pref n = sum.(map (pk n)) $ [1..n] --sum up all the partitions with [1..n] parts

--Should be possible with dynamic programming, since after choosing the first
--split we have a smaller subproblem already calculated.
--
--The problem can be reformulated as counting the possible partitions of an integer n
--
--The bruteforce calculation (using memoziation) is too slow (30min+ and p(n)>4800000000)
--
--Thought 1: rewrite p(n) using the recurrence for p_k(n)
--

innerProd :: [Int64] -> [Int64] -> Int64
innerProd a b = sum $ zipProd a b
    where zipProd = zipWith (*)

--
-- Use the direct recurrent relation of p (and see if this is faster)
-- http://mathworld.wolfram.com/PartitionFunctionP.html (20)
--

p :: Int64 -> Int64
p 0 = 1
p 1 = 1
p 2 = 2
p 3 = 3
p 4 = 5
p 5 = 7
p 6 = 11
p 7 = 15
p 8 = 22
p 9 = 30
p 10 = 42
p 11 = 56
p 12 = 77
p 13 = 101
p 14 = 135
p 15 = 176
p 16 = 231
p 17 = 297
p 18 = 385
p 19 = 490
p 20 = 627
p 21 = 792
p 22 = 1002
p 23 = 1255
p 24 = 1575
p 25 = 1958
p 26 = 2436
p 27 = 3010
p 28 = 3718
p 29 = 4565
p 30 = 5604
p 31 = 6842
p 32 = 8349
p 33 = 10143
p 34 = 12310
p 35 = 14883
p 36 = 17977
p 37 = 21637
p 38 = 26015
p 39 = 31185
p 40 = 37338
p 41 = 44583
p 42 = 53174
p 43 = 63261
p 44 = 75175
p 45 = 89134
p 46 = 105558
p 47 = 124754
p 48 = 147273
p 49 = 173525
p n = -(innerProd coefs (pn gpentasUsed)) --negate and move over all non-p(n)
    where gpentasUsed = tail $ takeWhile (<=n) gpentas
          coefs       = tail $ kths
          pn          = map (p.(n-))

penta :: Int64 -> Int64
penta n = div (n*(3*n-1)) 2

gpenta :: Int64 -> Int64
gpenta n = penta m 
    where m 
            | odd n = div n 2 + 1 
            | otherwise = - div n 2

gpentas :: [Int64]
gpentas = map gpenta [0..]

kth :: Int64 -> Int64
kth k
        | even power =  1
        | otherwise  = -1
        where power = div (k+1) 2

kths :: [Int64]
kths = map kth [0..] 

pk :: Int64 -> Int64 -> Int64
--partition of n with k parts
--assumes ``and [n>=0,k>=0]''
pk 0 0 = 1
pk _ 1 = 1
pk n k -- n,k>=2
    | n==k                  = 1
    | k>n                   = 0 -- zero ways to partion into more than actual parts
    | otherwise             = ( pk (n-1) (k-1) ) + ( pk (n-k) k )
--Haskell => Memoized => Dynamic programming

divisible :: Int64 -> Int64 -> Bool
divisible n = (==0).(flip rem n)

--main = print $ head.filter (divisible 1000000) $ map p [1..]
main = (mapM_ print) $ map (\n->(n,pUsed n)) [1..80]
    where pUsed = p

