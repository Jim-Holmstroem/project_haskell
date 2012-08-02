import Digit

fibs = 1:2:zipWith (+) fibs (tail fibs)

-- +1 for the missing starting F_1
main = print . (+1) .length . (takeWhile  ((<=1000).(length).(digits)) ) $ fibs
