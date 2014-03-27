import Data.List (minimumBy, sort)
import Data.Function (on)
import Data.Ord (comparing)
import Digit (digits)
import Prime (euler)

range = [2..10^7-1]

isPermutation (n, en) = (invariance n) == (invariance en)  -- (==) `on` sort . digits
    where invariance = sort . digits . fromIntegral

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral

main = print . fst . minimumBy (comparing snd) . map (\(n, en) -> (n, divide n en)) . filter isPermutation . map (\n -> (n, euler n)) $ range
