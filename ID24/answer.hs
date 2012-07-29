
import Data.List
-- NOTE perhaps doing it backwards (still the same answer by symmetri reasons)

permutationDigits = concat.(map show) $ [0..9]

main = print . (!!(1000000-1)). sort . permutations $ permutationDigits

