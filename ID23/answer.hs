import NumberTheory
import List

-- 28123, provable that it doesnt exist larger numbers then this.

-- addition is commutative

maximal = 28123

possible = (filter isAbundantNumber) $ [1..(div maximal 2)] --since the maximal used can only be maximal/2+maimal/2 (or smaller)

main = print.sum.nub $ [a+b|a<-possible,b<-possible,b<=a]


