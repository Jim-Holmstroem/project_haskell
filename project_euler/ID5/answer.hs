import Prime
import Data.List


divisors = [1..20]

divisorsPrimeFactors = map (primeFactors) divisors

-- groupBy (\x y-> (head x) == (head y)) $ sortBy (\x y-> compare (head x) (head y)) [[2,2],[3],[2,2,2],[4,4]]
sortgroupBy x = groupBy (\a b-> (head a)==(head b)) $ sortBy (\a b->compare (head a) (head b)) x --needs to be sorted before grouped

longestFactors x  =  sortgroupBy x  -- groupBy having the same prime


-- TODO howto avoid list in a list folding? http://twanvl.nl/blog/haskell/four-ways-to-fold

--main = print $ longestFactors 
main = print $ sortgroupBy $ [[2,2],[3,3]]++[[2,2,2],[3,3]] -- longestFactors [[2,2],[3,3]] [[2,2,2],[3]] -- $ map (group) divisorsPrimeFactors
