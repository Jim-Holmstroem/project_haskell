import Digit

main = putStr.(flip (++) "\n") . concat .(map show) .reverse. take 10 . reverse . digits . sum.(map (\x->x^x)) $ [1..1000]
