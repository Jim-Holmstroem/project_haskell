squaresum xs = sum $ map (^2) xs 
sumsquare xs = (^2) $ sum xs
diff [a,b] = a - b
main = print $  diff $ map ($ [1..100]) [sumsquare,squaresum]
