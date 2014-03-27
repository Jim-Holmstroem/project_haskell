import Data.List (maximumBy, sort)
import Data.Function (on)
import Data.Ord (comparing)
import Digit (digits)
import Prime (euler)

range = [1..10^6]

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral

main = print . fst . maximumBy (comparing snd) . map (\(n, en) -> (n, divide n en)) . map (\n -> (n, euler n)) $ range
