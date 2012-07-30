
-- Reduce the problem to "on which line do I go down" and represent those as a binary number. We need to have atleast one place which it goes down to be able to reach the bottom and additionally it can't go down on each line since it would render the path outside the rectangle.
width = 20
main = print $ 2^(width+1)-2
