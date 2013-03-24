import Data.List
import Data.List.Extras --cabal install -p list-extras

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

--takenh :: Sudoku Int -> Sudoku [Int]
--takenh = map (repeat9.nub) -- The zero is just ignored later, change nub to remove zeros

--takenv :: Sudoku Int -> Sudoku [Int]
--takenv = transpose.takenh.transpose 

--takenb :: Sudoku Int -> Sudoku [Int]
--takenb = (replicate2d 3).(map2d (foldl1 (++))).(map group3).(map transpose).group3

singeltonOrSubtraction2d :: Sudoku [Int] -> Sudoku [Int] -> Sudoku [Int]
singeltonOrSubtraction2d reference collisions = zipWith2d singeltonOrSubtraction reference collisions

singeltonOrSubtraction :: [Int] -> [Int] -> [Int] -- a type of zipWith
singeltonOrSubtraction reference collisions
    | length reference <= 1 = reference --also propagating if reference is impossible
    | otherwise = reference \\ collisions

filterSingelton :: Sudoku [Int] -> Sudoku [Int] --needs to have the NonSingeltons to be [] to preserve alignment
filterSingelton = map2d singeltonify
    where 
        singeltonify elem
            | length elem == 1 = elem
            | otherwise = []

filterNonCollisionBlock :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionBlock m = singeltonOrSubtraction2d m (collisions m)
    where collisions = (replicate2d 3).(map2d (concat.(map concat))).(map group3).(map transpose).group3.filterSingelton 

filterNonCollisionVertical :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionVertical m = singeltonOrSubtraction2d m (collisions m)
    where collisions = (map ((replicate 9).concat)).filterSingelton

filterNonCollisionHorizontal :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionHorizontal = transpose.filterNonCollisionVertical.transpose

filterNonCollision :: Sudoku [Int] -> Sudoku [Int]
filterNonCollision = filterNonCollisionBlock.filterNonCollisionVertical.filterNonCollisionHorizontal

all2d :: Int -> [[Bool]] -> Bool
all2d d = (all1d).(map all1d)
    where all1d = all id

fullfilled2d :: ([Int] -> Bool) -> Sudoku [Int] -> Bool
fullfilled2d f = (all2d 2).(map2d f)

solved :: Sudoku [Int] -> Bool
solved = fullfilled2d ((==1).length) 

valid :: Sudoku [Int] -> Bool
valid = fullfilled2d ((/=0).length) 

possible :: Sudoku Int -> Sudoku [Int]
possible  = map2d possibleElement 
    where 
        possibleElement elem
            | elem /=0 = [elem]
            | otherwise = omega

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

--collisions :: Sudoku Int -> [Sudoku [Int]]
--collisions sudoku = (map ($ sudoku) [takenh, takenv, takenb])

--occupied :: Sudoku Int -> Sudoku Bool
--occupied = map2d (/=0)

--numbers_left :: Sudoku Int -> Sudoku [Int]
--numbers_left = (map2d complement).(fold2d union).collisions

--possiblechoices :: Sudoku [Int] -> [Sudoku Int] 
--returns new sudokus with all possible moves (TODO ordered by 
--how possible they are (p \prop num_choices (a priori)))
--possiblechoices = sequence.(map sequence)

--solved :: Sudoku Int -> Bool
--solved = (all id).(map (all id)).occupied

--solve :: [Sudoku Int] -> [Sudoku Int]
--start sudoku in solved sudokus out (multiple)
--solve = (filter solved).(map solve).(foldl1 (++)).(map possiblechoices) should pass and return Sudoku [Int]

--elemtolist :: a -> [a] -- just a helper function to make a singleton list 
--elemtolist = (take 1).repeat

--initiate_solve :: Sudoku Int -> [Sudoku Int]
--initiate_solve = solve . elemtolist needs cleaning from Sudoku [Int] to Sudoku Int at returning

main = print.filterNonCollision.possible $ test_sudoku

