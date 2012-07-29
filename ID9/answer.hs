

-- a<b<c
-- a^2+b^2=c^2
-- a+b+c=n
--
-- c = n - (a+b)
-- 2n(a+b)-n^2-2ab=0 (along with a<-[1..] and b\in N, to get the search down to O(n))
-- 
-- a>0 since else would b^2=c^2 -> b=c (b,c>0) which is not valid from the first criteria

n=1000

isPytagoreanTriplet (a,b,c) = (==) ((+) (a^2) (b^2)) (c^2)
c a b = n - (a+b)
prod3 (a,b,c)=a*b*c --ugly but works
-- could do it faster by just finding a,b (without order) and the sort the output
main = print . (prod3 ) . head . (filter (isPytagoreanTriplet)) $ [(a,b,c a b)|a<-[1..n],b<-[a..n]]

