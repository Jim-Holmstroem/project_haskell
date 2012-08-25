
module Math
( diff
) where

diff::[Integer]->[Integer]
diff n = (map (\x->(-) (snd x) (fst x))) $ zip n (tail n) 

