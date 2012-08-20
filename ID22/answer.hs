import System.IO
import Data.Char
import Data.List
import Data.List.Split
import Char

load_wordlist::String->[String]
load_wordlist = (splitOn ",").(filter (/='"'))

point::String->Int
point = sum.(map ((+1).((+) (-(ord 'A'))).ord))

main = do
    contents <- getContents
    print.sum.(map (\v->(fst v)*(point.snd $ v))).(zip [1..]).sort.load_wordlist $ contents

