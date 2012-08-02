import Digit

fibs = 1:2:zipWith (+) fibs (tail fibs)

-- +1 for the missing starting F_1 (and +1 agains since the actual fibonccinumbers is just almost picked out (next in line))
main = print . (+1) . (+1) . length . (takeWhile  ((<1000).(length).(digits)) ) $ fibs
