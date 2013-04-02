

p :: Int -> Int
p 0 = 0
p 1 = 1
p 2 = 2
p 3 = 3
p 4 = 6
p 5 = 7

--p 0

--p 1
--0 -- p 0

--p 2
--00 -- p 0
--0 0 -- p 1

--p 3
--000 -- p 0
--00 0 -- p 1
--0  0 0 -- p 2 (all except last is a collision)

--p 4
--0000 -- p 0
--000 0 -- p 1
--00  00 -- p 2
--00  0 0
--0   0 0 0 -- p 3 (all except last is a collision)

--p 5
--00000 -- p 0
--0000 0 -- p 1
--000  00 -- p 2
--000  0 0
--00   00 0 -- p 3 (00 000 already exists)
--00   0 0 0
--0    0 0 0 0 -- p 4 (all except last is a collision)

--p 6
--000000 -- p 0
--00000 0 -- p 1
--0000  00 -- p 2
--0000  0 0
--000   000 -- p 3
--000   00 0
--000   0 0 0
--00    00 00 -- p 4 (a few configurations already exists)
--00    00 0 0
--00    0 0 0 0
--0     0 0 0 0 0 -- p 5 (all except last is a collision)

--Should be possible with dynamic programming, since after choosing the first
--split we have a smaller subproblem already calculated.

--Only the size of the piles defines the pile configuration
--thus one can sort the size of the piles

main = putStr $ "Hello Haskell\n"
