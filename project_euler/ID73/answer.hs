
import Data.Ratio
import Data.List

relativePrime x y = (gcd x y) == 1

m = 12000
a = 1%3
b = 1%2

main = print.length.(filter (>a)).(filter (<b)).nub $ [n%d|d<-[1..m],n<-[1..(d-1)]]

