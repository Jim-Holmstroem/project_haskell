
module Digit
( digits
)

where

digits :: Integer -> [Int]
digits = map (read . (:[])) . show
--digits = map (read . return) . show
--digits n = map (\x -> read [x] :: Int) (show n)
