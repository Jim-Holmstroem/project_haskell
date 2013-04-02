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
p 3 = 3
p 4 = 5
p 5 = 7
p n = sum.(map (pk n)) $ [1..n] --sum up all the partitions with [1..n] parts

--Should be possible with dynamic programming, since after choosing the first
--split we have a smaller subproblem already calculated.
--
--The problem can be reformulated as counting the possible partitions of an integer n
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

main = (mapM_ print) $ map p $ [1..100]
