import qualified Prime (primes, isPrime)


primes_up_to :: Int -> [Int]
primes_up_to k = takeWhile (<=k) Prime.primes


-- Number of splits with maximum of k summing up to n
n_splits_up_to_k :: Int -> Int -> Int
n_splits_up_to_k n k
    | n == 0 = 1 -- valid combination (k always prime if this is true
    | n < 2 = 0 -- not a valid combination
    | otherwise = sum.(map (subsplit n)).primes_up_to $ k
        where subsplit n ki = n_splits_up_to_k (n - ki) ki


-- Number of splits of n
n_splits :: Int -> Int
n_splits 0 = 0
n_splits 1 = 0
n_splits 2 = 1
n_splits n = n_splits_up_to_k n n

condition :: Int -> Bool
condition = (>5000).n_splits
--debug = (mapM (\x->print (x,n_splits x))) $ [1..]
--main = debug
main :: IO ()
main = print.head.(filter condition) $ [2..]
