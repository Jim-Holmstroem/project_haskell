
module Digit
( digits
)

where

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)
