-- NOTE, smarter to use (\sum a_i)^2 - \sum a_i^2 = 2\sum_{i\ne j} a_i*a_j (or is it?)

squaresum xs = sum $ map (^2) xs 
sumsquare xs = (^2) $ sum xs
diff [a,b] = a - b
main = print $  diff $ map ($ [1..100]) [sumsquare,squaresum]
