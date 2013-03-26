import Data.List
import Data.Ord
type Sudoku k = [[k]]
width = 9
omega = [1..width]
testSudoku :: Sudoku Int
testSudoku = [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]

-- 003020600
-- 900305001
-- 001806400
-- 008102900
-- 700000008
-- 006708200
-- 002609500
-- 800203009
-- 005010300

-- [HELPERS]

all2d :: Int -> [[Bool]] -> Bool
all2d d = (all1d).(map all1d)
    where all1d = all id

fullfilled2d :: ([Int] -> Bool) -> Sudoku [Int] -> Bool
fullfilled2d f = (all2d 2).(map2d f)

replicate2d :: Int -> Sudoku a -> Sudoku a
replicate2d i = (foldl1 (++)).(map (replicate i)).(map (foldl1 (++))).(map2d (replicate i))

group3 :: [a] -> [[a]] -- [1,2,3,4,5,6] -> [[1,2,3],[4,5,6]]
group3 [] = []
group3 (a:[]) = error "len%3==1"
group3 (a:b:[]) = error "len%3==2"
group3 (a:b:c:xs) = [a,b,c] : group3 xs

complement :: [Int] -> [Int] 
complement = (\\) omega 

map2d :: (a -> b) -> Sudoku a -> Sudoku b -- elementvise operation on Sudoku
map2d f = map (map f) --do this with a 2 foldr and apply or something

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d f = zipWith (zipWith f)

fold2d :: (a -> a -> a) -> [Sudoku a] -> Sudoku a --folds sudokus
fold2d f = foldl1 (zipWith2d f)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [[((Int,Int), a)]]
enumerate2d = (map outer).enumerate 
    where outer = \row@(rowIndex, rowValue)->(map (inner row)).enumerate $ rowValue
          inner = \row@(rowIndex, rowValue) col@(colIndex, colValue)->((rowIndex, colIndex), colValue) 

argminIndex :: Ord b => (a -> b) -> [a] -> Int --note that the domain in this case is the index-domain
argminIndex f = fst.(minimumBy (comparing (f.snd))).(zip [0..]) 

argminIndex2d :: Ord b => (a -> b) -> [[a]] -> (Int, Int)
argminIndex2d f m = (rowArgmin, argminIndex f rowMin)
    where rowMin = m !! rowArgmin
          rowArgmin = argminIndex (minimum.(map f)) m

-- [SUDOKU]

solved :: Sudoku [Int] -> Bool
solved = fullfilled2d ((==1).length) 

valid :: Sudoku [Int] -> Bool
valid = fullfilled2d ((/=0).length) 

certain :: Sudoku [Int] -> Sudoku Int
certain = map2d head 

singeltonOrSubtraction2d :: Sudoku [Int] -> Sudoku [Int] -> Sudoku [Int]
singeltonOrSubtraction2d reference collisions = zipWith2d singeltonOrSubtraction reference collisions

singeltonOrSubtraction :: [Int] -> [Int] -> [Int] -- a type of zipWith
singeltonOrSubtraction reference collisions
    | length reference <= 1 = reference --also propagating if reference is impossible
    | otherwise = reference \\ collisions

possible :: Sudoku Int -> Sudoku [Int]
possible  = map2d possibleElement 
    where 
        possibleElement elem
            | elem /=0 = [elem]
            | otherwise = omega

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
    where collisions = (map ((replicate width).concat)).filterSingelton

filterNonCollisionHorizontal :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionHorizontal = transpose.filterNonCollisionVertical.transpose

takeAtLeastOneWhile :: (a->Bool) -> [a] -> [a]
takeAtLeastOneWhile f l = head l : takeWhile f l

iterateWhileNonStationary :: (Eq a) => (a->a) -> a -> a -- f^n ( a ) until it is stationary
iterateWhileNonStationary f a = snd.last.(takeAtLeastOneWhile (not.stationary)) $ updates
    where updates = zip fn (tail fn) 
          fn = iterate f a
          stationary (last, current) = last == current

filterNonCollision :: Sudoku [Int] -> Sudoku [Int]
filterNonCollision = iterateWhileNonStationary (block.vertical.horizontal)
    where block = filterNonCollisionBlock
          vertical = filterNonCollisionVertical
          horizontal = filterNonCollisionHorizontal

fixationsSmallest :: Sudoku [Int] -> [Sudoku [Int]] --finds the smallest non-singelton list and fixate it foreach element in it 
fixationsSmallest m = map (\fixationElem->(map2d (fixate indexArgmin fixationElem)).enumerate2d $ m)  valueMin
    where indexArgmin@(rowArgmin, colArgmin) = argminIndex2d lengthLongerThanOne m
          valueMin = (m!!rowArgmin)!!colArgmin
          lengthLongerThanOne elem
            | length elem > 1 = length elem 
            | otherwise = maxBound::Int
          fixate index fixationElem elem --transparent if it's not the element to fixate
            | index == (fst elem) = [fixationElem] 
            | otherwise = snd elem

solve :: Sudoku [Int] -> [Sudoku [Int]] --pushes valid not solved (and solved)
solve m 
    | solved m      = [m]
    | (not.valid) m = [ ]
    | otherwise     =  m : possibleChoices m
    where possibleChoices = concat.(map nextStep).fixationsSmallest.filterNonCollision
          nextStep = solve.filterNonCollision

solutions :: Sudoku Int -> [Sudoku Int]
solutions = postprocess.(filter solved).solve.prepare
    where prepare = filterNonCollision.possible
          postprocess = map certain

parse :: [String] -> [Sudoku Int]
parse [] = []
parse (_:a:b:c:d:e:f:g:h:i:rest) = (map parseLine $ a:b:c:d:e:f:g:h:i:[]) : parse rest
    where parseLine = map (\d->read [d]::Int) 

magicNumber :: Sudoku Int -> Int
magicNumber m = read (concat.(map show).(take 3).head $ m) ::Int

main = do
    contents <- getContents
    print.sum.(map magicNumber).(map (head.solutions)).parse.lines.cleanInput $ contents
        where cleanInput = filter (/='\r')
