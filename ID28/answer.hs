
-- dimensions on shell n
l = (+1).(2*) --length
a = (^2).l -- area is the same as the last number when your starting at level n+1

shellsum n 
    | n>0   = 4*((a (n-1)) + 5*n) -- a(n-1)+5/2*(l(a)-1), expression dereived
    | n==0  = 1 --specialcase is special
    | n<0   = error "too small"

n = (flip (div) 2).((+) (-1)) $ 1001 -- last level

main = print . sum . (map shellsum) $ [0..n]

