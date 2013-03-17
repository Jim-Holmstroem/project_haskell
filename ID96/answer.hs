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
--takenb = 

group3 :: [a] -> [[a]] -- [1,2,3,4,5,6] -> [[1,2,3],[4,5,6]]
group3 [] = []
group3 (a:[]) = error "len%3==1"
group3 (a:b:[]) = error "len%3==2"
group3 (a:b:c:xs) = [a,b,c] : group3 xs

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
numbers_left = (map2d complement).(fold2d union).collisions

main = print.(map2d (foldl1 union)).(map group3).(map transpose).group3 $ test_sudoku

