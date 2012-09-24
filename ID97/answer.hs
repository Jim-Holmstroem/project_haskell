import Digit
main = print . (read::String -> Integer) . (foldl1 (++)) . (map show) . reverse . (take 10) . reverse . digits $ 28433*2^7830457+1
