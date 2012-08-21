import NumberTheory

-- d(a) = b, d(b) = a, a/=b <==>
-- d(a) /= a, d^2(a) = a 

d = sum . properDivisors

amicable = (filter (\a->(d.d$a)==a)).(filter (\a->(d a)/=a))

main = print . sum . amicable $ [1..(10000-1)]
