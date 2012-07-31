
import Digit

-- Using letters and then count them to get it easier to debug

zero2nine    =["","one","two","three","four","five","six","seven","eight","nine"] -- you never say zero in counting except for zero
ten2nineteen =["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]



-- as a function to avoid
tens::Int->String
tens c
    | c == 2 = "twenty"
    | c == 3 = "thirty"
    | c == 4 = "fourty"
    | c == 5 = "fifty"
    | c == 6 = "sixty"
    | c == 7 = "seventy"
    | c == 8 = "eighty"
    | c == 9 = "ninety"
    | otherwise = error "that tens is out of range"

hundred = 7
thousand = 8
andc = 3

numberOfLetters::Int->String
numberOfLetters c
    | c == 0                        = error "zero permitted"
    | c < 10 = zero2nine!!c -- [0..9]
    | 10 <= c && c < 20             = ten2nineteen!!(c-10) -- [10..19]
    | c<100 && (mod c 10)==0        = tens (div c 10) -- [20,30..90]
    | c<100 && (mod c 10)/=0        = tens (div c 10) + numberOfLetters (c - 10*(div c 10)) -- <100 (not listed above)
    | c<1000 && (mod c 100)==0      = numberOfLetters (div c 100) + hundred
    | c<1000 && (mod c 100)/=0      = numberOfLetters (div c 100) + hundred + andc + numberOfLetters (c - 100*(div c 100))
    | c==1000                       = numberOfLetters 1 + thousand

    | otherwise                     = error "Range not implemented"

main = print . sum. (map (numberOfLetters)) $ [1..1000]
