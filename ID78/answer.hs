import Debug.Trace
import Control.Parallel
import Control.Parallel.Strategies

innerTrace :: (Show a) => [Char] -> a -> a
--innerTrace message a = a 
innerTrace message a = trace (message ++ ":" ++ show a) a 

p :: Int -> Int
p 0 = 1
p 1 = 1
p 2 = 2
p n = sum.(map (pk n)) $ [1..n] --sum up all the partitions with [1..n] parts

--Should be possible with dynamic programming, since after choosing the first
--split we have a smaller subproblem already calculated.
--
--The problem can be reformulated as counting the possible partitions of an integer n
--
--The bruteforce calculation (using memoziation) is too slow (30min+ and p(n)>4800000000)
--
--Thought 1: rewrite p(n) using the recurrence for p_k(n)
--

pk :: Int -> Int -> Int
--partition of n with k parts
--assumes ``and [n>=0,k>=0]''
pk 0 0 = 1
pk _ 1 = 1
pk n k -- n,k>=2
    | n==k                  = 1
    | k>n                   = 0 -- zero ways to partion into more than actual parts
    | otherwise             = ( pk (n-1) (k-1) ) + ( pk (n-k) k )
--Haskell => Memoized => Dynamic programming

divisible :: Int -> Int -> Bool
divisible n = (==0).(flip rem n)

--main = print $ head.filter (divisible 1000000) $ map p [1..]
main = (mapM_ print) $ map p [1..]

