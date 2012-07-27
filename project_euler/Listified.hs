
module Listified
(
slidedList
) where

import Data.List

-- slides the list with the length l
slidedList l x = filter ((==l).(length)) $ transpose $ map ($ x) $ map (drop) [0..(l-1)]

