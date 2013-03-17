import Data.List

type Sudoku k = [[k]]
omega = [1..9]
test_sudoku :: Sudoku Int
test_sudoku = [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]

-- 003020600
-- 900305001
-- 001806400
-- 008102900
-- 700000008
-- 006708200
-- 800203009
-- 005010300

takenh :: Sudoku Int -> Sudoku [Int]
takenh = map ((take 9).repeat.nub) -- The zero is just ignored later, change nub to remove zeros

takenv :: Sudoku Int -> Sudoku [Int]
takenv = transpose.takenh.transpose 

--takenb :: Sudoku Int -> Sudoku [Int]
--takenb = groupby 3 rows --taken in the box

--group3by3 = transpose.group3.transpose.group3 --group the actual sudoku returns a 3x3x9 tensor
--group3 = iterate somehow ?--group in one direction 

complement :: [Int] -> [Int] -- Omega\Union_i(collision_i)
complement = (\\) omega 

map2d :: (a -> b) -> Sudoku a -> Sudoku b -- elementvise operation on Sudoku
map2d f = map (map f) --do this with a 2 foldr and apply or something

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d f = zipWith (zipWith f)

fold2d :: (a -> a -> a) -> [Sudoku a] -> Sudoku a --folds sudokus
fold2d f = foldl1 (zipWith2d f)

collisions :: Sudoku Int -> [Sudoku [Int]]
collisions sudoku = map ($ sudoku) [takenh, takenv]

numbers_left :: Sudoku Int -> Sudoku [Int]
numbers_left sudoku = (map2d complement).(fold2d union).collisions $ sudoku

main = print..numbers_left $ test_sudoku

