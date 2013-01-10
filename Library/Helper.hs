
module Helper
( compareBy 
) where

import Data.List

compareBy:: Ord b => (a->b) -> a -> a -> Ordering
compareBy f x y = f x `compare` f y

count:: Ord a => [a] -> [(a,Int)]
count = (map (\xs->(head xs, length xs))).group.sort

