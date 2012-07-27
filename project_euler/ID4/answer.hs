import Prime
import Digit
import Prime
import Data.List

num_digits = 3


-- palindromIt n = read $ ( foldl1 (++) ((map (show) $ digits n) ++ (map (show) $ reverse $ digits n)))::Integer

isPalindromList [] = True
isPalindromList (n:[]) = True --to deal with odd length
isPalindromList n = ((head n) == (last n)) && (isPalindromList $ init $ tail n) --

isPalindromNumber n = isPalindromList $ digits n

biggest = (10^num_digits-1)
smallest = 10^(num_digits-1)
possible = reverse $ [smallest..biggest]

main = print $ take 1 $ filter ((isPalindromNumber).(!!0)) $ reverse $ sort $ [[a*b,a,b]|a<-possible,b<-possible,a<=b] --ordo biggest^2/2 

--main = print $ take 1 $ map ((primeFactors).(palindromIt)) possible
--main = print $ take 1 $ filter (\x->( length $ primeFactors x)>1) $ palindromIt $ reverse [1..num] -- NOTE if it does have more then 2 primefactors you can always  make 2 non-primefactors from the primes
