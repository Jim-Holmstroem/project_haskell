
import NumberTheory

nmax=100
nrange = [1..nmax]
criteria = (>1000000)

firstOver = head . (filter (criteria.largestCombination)) $ nrange
  where
    largestCombination = (\n->nCr n (div n 2)) -- largest combination with "n" elements

-- map largestCombination [1..] is stricly increasing we know that all lower n will not satisfy the criteria in nCr

main = print . length . (filter criteria) $ [nCr n r|n<-[firstOver..nmax],r<-[1..n]]

