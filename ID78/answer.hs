import Debug.Trace

innerTrace :: (Show a) => [Char] -> a -> a
--innerTrace message a = a 
innerTrace message a = trace (message ++ ":" ++ show a) a 

p :: Int -> Int
p 0 = 0
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
pk n k
    | n==k                  = 1
    | k>n                   = error ("k>n:"++(show k)++" "++(show n))
    | otherwise             =
         ( innerTrace "pk1" $ 
            pk (
                innerTrace "    n=n-1" (n-1)
            ) (
                innerTrace "    k=k-1" (k-1)
            )
         ) + 
         ( innerTrace "pk2" $
            pk (
                innerTrace "    n=n-k" (n-k)
            ) (
                innerTrace "    k=k" (k)
            )
         )

main = putStr $ "Hello Haskell\n"
