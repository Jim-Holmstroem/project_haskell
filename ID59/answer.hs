import System.IO
import Data.List
import Data.List.Split
import Data.Bits
import Data.Char
import Control.Monad
import Helper
-- Install Split by: cabal install split

count = (map (\xs -> (head xs, length xs))).group.sort
-- unigram analysis
-- e is the most common letter in the english language 
-- with 12.7% compared to 9.1%
ascii_e = ord 'e'

main = do 
    message <- getLine 
    let tokens = (map (read::String->Int)) . (splitOn ",") $ message
    print.(map chr) $ tokens
    let freq = count tokens
    forM [1..100] (\a -> do
        let e_encrypted = fst.(!!a).reverse.(sortBy (compareBy snd)) $ freq
        let key = xor e_encrypted ascii_e
        let decrypted_tokens = map (xor key) tokens
        let decrypted_message = map chr decrypted_tokens
        print $ key
        print.(map (chr.(xor key).fst)).reverse.(sortBy (compareBy snd)) $ freq
        )
    need to do a cycle, thats why we have a problem..
        --print decrypted_message)
    print "end"
