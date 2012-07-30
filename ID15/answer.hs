
-- Represnt tha path as an seqeuence of valid steps, 
-- the manhattan distance will always be width+height and (d_1x = width,d_1y=height)
-- example with Right=R,Down=D: RDDRRDDRRDDRRDDRRDDRRDDRRDDRRDDRRDDRRDDR
-- The number of possible combinations with these restrictions, having a total of 40 letters of 20 each gives us the well known count 40 choose 20.

import NumberTheory

width = 20
height= 20
main = print $ nCr (height+width) width --(or nCr (height+width) height)
