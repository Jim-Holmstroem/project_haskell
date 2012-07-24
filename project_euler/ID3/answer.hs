import Prime

main = print $ foldl1 max $ primeFactors 600851475143

