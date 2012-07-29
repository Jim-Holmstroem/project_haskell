import operator as op

def nCr(n, r):
    r = min(r, n-r)
    if r == 0: return n
    num = reduce(op.mul, xrange(n, n-r, -1))
    denom = reduce(op.mul, xrange(1, r+1))
    return num//denom


# Just realized that the algorithm will still fall on nCr
# count all over 1000000

M = 1000000

N = 100 #the ground level

def pyramidA(n):
    if(not (n%2==0)):
        return pyramidA(n-1)-1 #the cut off pyramid (+1 bigger but without the tip)
    height = N-n
    return height*(height+1)/2


def shellA(n): #start getting the shell
    def shelllevel(n): #area of only one shelllevel starting at n
        pass
    pass
    

# missing "head $ filter ((<100000).(nCr)) [list], abandoning




pyramid = pyramidA(23) 
