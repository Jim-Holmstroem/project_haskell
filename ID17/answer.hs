import Digit
import Data.Char

-- Using letters and then count them to get it easier to debug

zero2nine    =[   "",   "one",   "two",   "three",    "four",   "five",    "six",    "seven",   "eight",    "nine"] -- you never say zero in counting except for zero
ten2nineteen =["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]

-- as a function to avoid
tens::Int->String
tens c
    | c == 2 = "twenty"
    | c == 3 = "thirty"
    | c == 4 = "forty"
    | c == 5 = "fifty"
    | c == 6 = "sixty"
    | c == 7 = "seventy"
    | c == 8 = "eighty"
    | c == 9 = "ninety"
    | otherwise = error "that tens is out of range"

hundred = "hundred"
thousand = "thousand"
andc = "and"

number2Letters::Int->String
number2Letters c
    | c == 0                        = error "zero not permitted"
    | c < 10                        = zero2nine!!c -- [0..9]
    | 10 <= c && c < 20             = ten2nineteen!!(c-10) -- [10..19]
    | c<100 && (mod c 10)==0        = tens (div c 10) -- [20,30..90]
    | c<100 && (mod c 10)/=0        = tens (div c 10) ++ "-" ++ number2Letters (c - 10*(div c 10)) -- <100 (not listed above)
    | c<1000 && (mod c 100)==0      = number2Letters (div c 100) ++ " " ++ hundred
    | c<1000 && (mod c 100)/=0      = number2Letters (div c 100) ++ " " ++ hundred ++ " " ++ andc ++ " "  ++ number2Letters (c - 100*(div c 100))
    | c==1000                       = number2Letters 1 ++ " " ++ thousand
    | otherwise                     = error "Range not implemented"

--main = putStr . unlines . (map (number2Letters)) $ [1..1000] --debugging
main = print . length . (filter (isLetter)) .concat. (map (number2Letters)) $ [1..1000]
