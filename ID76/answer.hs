-- same basic idea as ID77

-- Number of splits with maximum of k summing up to n
n_splits_up_to_k :: Int -> Int -> Int
n_splits_up_to_k n k
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = sum . map (subsplit n) $ [1..k]
        where subsplit n ki = n_splits_up_to_k (n - ki) ki


-- Number of splits of n
n_splits :: Int -> Int
n_splits 0 = 0
n_splits 1 = 0
n_splits 2 = 1
n_splits n = n_splits_up_to_k n n - 1


main :: IO ()
main = print . n_splits $ 100
