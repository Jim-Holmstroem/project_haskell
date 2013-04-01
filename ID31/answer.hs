import Data.List

total :: Int
total = 200

values :: [Int]
values = [1,2,5,10,20,50,100,200]

-- The collection of coins should be a data type with an amount function

--Find the bounds foreach coin and bruteforce through that space

bounds :: [Int] -> [Int]
bounds = map (quot total)

productN :: [Int] -> [[Int]]
productN = sequence.(map range)
    where range n = [0..n]

innerProd :: [Int] -> [Int] -> Int --inner product
--double curry?
innerProd a b = sum $ zipWith (*) a b

--numSolutions :: Int
--trivial solution
--numSolutions = length.filter ((==total).(innerProd values)).productN $ bounds values

--ignore the 1-ones and just add those to get to ``total''
numSolutions = length.filter ((<=total).innerProd (tail values)).productN $ bounds (tail values)


main = print numSolutions
