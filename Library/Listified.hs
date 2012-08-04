
module Listified
(slidedList
,testmatrix
,continousSubSeqs
--,printMatrix
) where

import Data.List

-- slides the list with the length l
slidedList l x = filter ((==l).(length)) $ transpose $ map ($ x) $ map (drop) [0..(l-1)]

testmatrix = [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20],[21,22,23,24,25]]
--printMatrix::[a]->IO()
--printMatrix = putStr.unlines.(map (show)) --for some reason doesn't work copy it

continousSubSeqs = filter (not . null) . concatMap inits . tails
