import Digit

main = putStr . concat .(map show) . take 10 . reverse . digits . sum.(map (\x->x^x)) $ [1..1000]
