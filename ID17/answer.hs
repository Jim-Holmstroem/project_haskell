
import Digit

zero2nine    =[4,3,3,5,4,4,3,5,5,4]
ten2nineteen =[3,6,6,8,8,7,7,9,9,8]

-- as a function to avoid
tens::Int->Int
tens c
    | c == 2 = 6
    | c == 3 = 6
    | c == 4 = 5
    | c == 5 = 5
    | c == 6 = 5
    | c == 7 = 7
    | c == 8 = 6
    | c == 9 = 6
    | otherwise = error "that tens is out of range"

hundred = 7
thousand = 8
andc = 3

numberOfLetters::Int->Int
numberOfLetters c
    | c < 10 = zero2nine!!c -- [0..9]
    | 10 <= c && c < 20             = ten2nineteen!!(c-10) -- [10..19]
    | c<100 && (mod c 10)==0        = tens (div c 10) -- [20,30..90]
    | c<100 && (mod c 10)/=0        = tens (div c 10) + numberOfLetters (c - 10*(div c 10)) -- <100 (not listed above)
    | c<1000 && (mod c 100)==0      = numberOfLetters (div c 100) + hundred
    | c<1000 && (mod c 100)/=0      = numberOfLetters (div c 100) + hundred + andc + numberOfLetters (c - 100*(div c 100))
    | c==1000                       = numberOfLetters 1 + thousand

    | otherwise                     = error "Range not implemented"

main = print .(!!(1000-1)) . (map (numberOfLetters)) $ [1..1000]
