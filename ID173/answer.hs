
area l d = 4*d*(l+d) -- area of a laminae with inner width L and hullradius d, NOTE the representation is unique and fills the laminae-space
m=1000000

test l = length $ takeWhile ((<=m).(area l)) [1..] --varies d and returns the number of possible

main = print $ sum $ takeWhile (>0) $ map (test) [1..] --(takeWhile "test still returns possible laminaes with given L")

