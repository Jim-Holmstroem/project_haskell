import Digit
import Data.List

-- Observation 1: the product must have 4 digits (3 is too less and 5 too much)

one2nine = [1..9]

pick n = nub . (map (take n)) . permutations 

leftovers = (\\) one2nine

products = pick 4 one2nine

main = print 

