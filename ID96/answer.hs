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
-- 002609500
-- 800203009
-- 005010300

takenh :: Sudoku Int -> Sudoku [Int]
takenh = map ((take 9).repeat.nub) -- The zero is just ignored later, change nub to remove zeros

takenv :: Sudoku Int -> Sudoku [Int]
takenv = transpose.takenh.transpose 

takenb :: Sudoku Int -> Sudoku [Int]
takenb = (replicate2d 3).(map2d (foldl1 (++))).(map group3).(map transpose).group3

replicate2d :: Int -> Sudoku a -> Sudoku a
replicate2d i = (foldl1 (++)).(map (replicate i)).(map (foldl1 (++))).(map2d (replicate i))

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
collisions sudoku = (map ($ sudoku) [takenh, takenv, takenb])

occupied :: Sudoku Int -> Sudoku Bool
occupied = map2d (/=0)

numbers_left :: Sudoku Int -> Sudoku [Int]
numbers_left = (map2d complement).(fold2d union).collisions

possiblechoices :: Sudoku [Int] -> [Sudoku Int] 
--returns new sudokus with all possible moves (TODO ordered by 
--how possible they are (p \prop num_choices (a priori)))
possiblechoices = sequence.(map sequence)

solved :: Sudoku Int -> Bool
solved = (all id).(map (all id)).occupied

solve :: [Sudoku Int] -> [Sudoku Int]
--start sudoku in solved sudokus out (multiple)
solve = (filter solved).(map solve).(foldl1 (++)).(map possiblechoices) should pass and return Sudoku [Int]

elemtolist :: a -> [a] -- just a helper function to make a singleton list 
elemtolist = (take 1).repeat

initiate_solve :: Sudoku Int -> [Sudoku Int]
initiate_solve = solve . elemtolist needs cleaning from Sudoku [Int] to Sudoku Int at returning

main = print.(take 1).initiate_solve $ test_sudoku

