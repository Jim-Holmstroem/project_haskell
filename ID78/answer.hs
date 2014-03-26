import Data.Function.Memoize (memoize, memoize2)
-- same basic idea as ID77

-- cabal install memoize
-- cabal install --reinstall -p libraryname -- for profiling libraries
-- Number of splits with maximum of k summing up to n
n_splits_up_to_k :: Integer -> Integer -> Integer
n_splits_up_to_k n k
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = sum . (memoize (map (subsplit n))) $ [1..k]
        where subsplit n ki = (memoize2 n_splits_up_to_k) (n - ki) ki


-- Number of splits of n
n_splits :: Integer -> Integer
n_splits 0 = 0
n_splits 1 = 0
n_splits 2 = 1
n_splits n = n_splits_up_to_k n n


condition :: Integer -> Bool
condition = (== 0) . (\n -> mod n 1000000)

-- [2..256]
debug = mapM_ (\x -> print (x, mod x 1000000)) . takeWhile (not . condition) . map n_splits $ [2..]
main :: IO ()
--main = print . head . filter condition . map n_splits $ [2..]
main = debug
