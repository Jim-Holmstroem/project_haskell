import System.IO
import Data.List
import Data.List.Split
import Data.Bits
import Helper
-- Install Split by: cabal install split

count = (map (\xs -> (head xs, length xs))).group.sort
-- unigram analysis
-- e is the most common letter in the english language 
-- with 12.7% compared to 9.1%

main = do 
    message <- getLine 
    let tokens = (map (read::String->Int)) . (splitOn ",") $ message
    let freq = count tokens
    print.head.reverse.(sortBy (compareBy snd)) $freq
    
