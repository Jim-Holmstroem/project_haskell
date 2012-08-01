
module Digit
( digits
)

where

import Data.Char

digits :: Integer -> [Int]
--digits = map (read . (:[])) . show (slower but cooler)
digits = map digitToInt.show

