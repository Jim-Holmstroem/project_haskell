import Data.List
import Data.Ord

import Debug.Trace

import Control.Parallel
import Control.Parallel.Strategies

type Sudoku k = [[k]]
width :: Int
width = 9
omega = [1..width]

-- [DEBUGGING]

testSudoku0 :: Sudoku Int
testSudoku0 = [[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]
              ,[0,0,0,0,0,0,0,0,0]]
testSudoku1 :: Sudoku Int
testSudoku1 = [[0,0,3,0,2,0,6,0,0]
              ,[9,0,0,3,0,5,0,0,1]
              ,[0,0,1,8,0,6,4,0,0]
              ,[0,0,8,1,0,2,9,0,0]
              ,[7,0,0,0,0,0,0,0,8]
              ,[0,0,6,7,0,8,2,0,0]
              ,[0,0,2,6,0,9,5,0,0]
              ,[8,0,0,2,0,3,0,0,9]
              ,[0,0,5,0,1,0,3,0,0]]
testSudoku2 :: Sudoku Int
testSudoku2 = [[2,0,0,0,8,0,3,0,0]
              ,[0,6,0,0,7,0,0,8,4]
              ,[0,3,0,5,0,0,2,0,9]
              ,[0,0,0,1,0,5,4,0,8]
              ,[0,0,0,0,0,0,0,0,0]
              ,[4,0,2,7,0,6,0,0,0]
              ,[3,0,1,0,0,7,0,4,0]
              ,[7,2,0,0,4,0,0,6,0]
              ,[0,0,4,0,1,0,0,0,3]]
testSudoku3 :: Sudoku Int
testSudoku3 = [[0,0,0,0,0,0,9,0,7]
              ,[0,0,0,4,2,0,1,8,0]
              ,[0,0,0,7,0,5,0,2,6]
              ,[1,0,0,9,0,4,0,0,0]
              ,[0,5,0,0,0,0,0,4,0]
              ,[0,0,0,5,0,7,0,0,9]
              ,[9,2,0,1,0,8,0,0,0]
              ,[0,3,4,0,5,9,0,0,0]
              ,[5,0,7,0,0,0,0,0,0]]

lenprint = sprint.map2d length
sprint = mapM_ print
innertrace :: (Show a) => [Char] -> a -> a
innertrace message a = trace (message++":\n "++show a) a

innertracesudoku :: (Show a) => [Char] -> [[a]] -> [[a]]
innertracesudoku message a = trace 
                 (
                    message++":\n"
                    ++ (
                            foldr (\x y->x++"\n"++y) ""
                            .(map show) $ a
                       )
                 ) a

-- [HELPERS]

all2d :: Int -> [[Bool]] -> Bool
all2d d = all1d.map all1d
    where all1d = all id

fullfilled2d :: ([Int] -> Bool) -> Sudoku [Int] -> Bool
fullfilled2d f = all2d 2.map2d f

replicate2d :: Int -> Sudoku a -> Sudoku a
replicate2d i = foldl1 (++)
                .map (replicate i)
                .map (foldl1 (++))
                .map2d (replicate i)

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
enumerate2d = map outer.enumerate 
    where outer = \row@(rowIndex, rowValue)
                ->map (inner row).enumerate 
                $ rowValue
          inner = \row@(rowIndex, rowValue) col@(colIndex, colValue)
                ->((rowIndex, colIndex), colValue) 

argminIndex :: Ord b => (a -> b) -> [a] -> Int
argminIndex f = fst.minimumBy (comparing (f.snd)).zip [0..]

argminIndex2d :: Ord b => (a -> b) -> [[a]] -> (Int, Int)
argminIndex2d f m = (rowArgmin, argminIndex f rowMin)
    where rowMin = m !! rowArgmin
          rowArgmin = argminIndex (minimum.map f) m

-- [SUDOKU]

solved :: Sudoku [Int] -> Bool
solved = fullfilled2d ((==1).length) 

valid :: Sudoku [Int] -> Bool
valid = fullfilled2d ((/=0).length)

certain :: Sudoku [Int] -> Sudoku Int
certain m 
    | solved m = map2d head m
    | otherwise = error "certain but not.solved" 

singeltonOrSubtraction2d :: Sudoku [Int] -> Sudoku [Int] -> Sudoku [Int]
singeltonOrSubtraction2d reference collisions 
    = zipWith2d singeltonOrSubtraction reference collisions

singeltonOrSubtraction :: [Int] -> [Int] -> [Int] -- a type of zipWith
singeltonOrSubtraction reference collisions
    | length reference <= 1 = reference 
    | otherwise = reference \\ collisions

hypothesesSpace :: Sudoku Int -> Sudoku [Int]
hypothesesSpace  = map2d possibleElement
    where 
        possibleElement elem
            | elem /=0 = [elem]
            | otherwise = omega

excludeNonSingelton :: Sudoku [Int] -> Sudoku [Int] 
--needs to have the NonSingeltons to be [] to preserve alignment
excludeNonSingelton = map2d singeltonify
    where 
        singeltonify elem
            | length elem == 1 = elem
            | otherwise = []

instances :: (Eq a) => a -> [a] -> Int
instances x [] = 0
instances x (y:ys)
    | x==y      = 1 + instances x ys
    | otherwise =     instances x ys

counter :: (Ord a) => [a] -> [(a,Int)]
counter = map count.group.sort
    where count = \x->(head x, length x)

hasSingeltonCopies :: (Ord a) => [[a]] -> Bool
--to make sure no copies has been made when filteringNonCollisions, in the 
--implementation where no higher order logic is used we can have in 
--fixationsSmallest that [3,6],[3,8],[3,8] --{fixation}--> [3],[3,8],[3,8]
-- --> [3].[8].[8] which is invalid, even if implemented one most prove that it
-- doesn't exist another type of higher order logic that allows for this invalid
-- matrix in any other way.

-- 
hasSingeltonCopies = (>0).length.copies 
    where copies          = collisions.counter.filterSingelton
          filterSingelton =         filter ((==1).length)
          collisions      = map fst.filter ( (>1).snd   )  

concat2d :: [[[a]]] -> [a]
concat2d = concat.map concat

concatBlock :: Sudoku [Int] -> Sudoku [Int]
concatBlock = map2d concat2d 
             .map group3
             .map transpose
             .group3

concatHorizontal :: Sudoku [Int] -> [[Int]]
concatHorizontal = map concat 

concatVertical :: Sudoku [Int] -> [[Int]]
concatVertical = transpose.concatHorizontal.transpose

filterNonCollisionBlock :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionBlock m = singeltonOrSubtraction2d m (collisions m)
    where collisions = replicate2d 3
                       .concatBlock
                       .excludeNonSingelton

filterNonCollisionVertical :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionVertical m = singeltonOrSubtraction2d m (collisions m)
    where collisions = map (replicate width)
                       .concatHorizontal
                       .excludeNonSingelton

filterNonCollisionHorizontal :: Sudoku [Int] -> Sudoku [Int]
filterNonCollisionHorizontal = transpose.filterNonCollisionVertical.transpose

takeAtLeastOneWhile :: (a->Bool) -> [a] -> [a]
takeAtLeastOneWhile f l = head l : takeWhile f (tail l)

iterateWhileNonStationary :: (Eq a) => (a->a) -> a -> a 
-- f^n ( a ) until it is stationary
iterateWhileNonStationary f a = snd.last
                                .takeAtLeastOneWhile (not.stationary)
                                $ updates
    where updates = zip fn (tail fn) 
          fn = iterate f a
          stationary (last, current) = last == current

--constraint propagation
filterNonCollision :: Sudoku [Int] -> Sudoku [Int]
filterNonCollision = iterateWhileNonStationary combined 
    where combined m = fold2d intersect $ map ($m) [
                                                    filterNonCollisionBlock
                                                   ,filterNonCollisionVertical
                                                   ,filterNonCollisionHorizontal
                                                   ]

fixationsSmallest :: Sudoku [Int] -> [Sudoku [Int]] 
--finds the smallest non-singelton list and fixate it 
--foreach element in it
--
--This is the bruteforce part, but one can also see it as testing a hypotethsis
fixationsSmallest m = map 
                          (\fixationElem
                            ->map2d (fixate indexArgmin fixationElem)
                              .enumerate2d $ m
                          )
                      $ valueMin
    where indexArgmin@(rowArgmin,colArgmin)=argminIndex2d lengthLongerThanOne m
          valueMin = (m!!rowArgmin)!!colArgmin
          lengthLongerThanOne elem
            | length elem > 1 = length elem 
            | otherwise = maxBound::Int
          fixate index fixationElem elem 
            | index == (fst elem) = [fixationElem] 
            | otherwise = snd elem

validityCheck :: Sudoku [Int] -> Bool
--Checks if the sudokus is still valid, see excludeSingeltonCopies 
validityCheck m = foldl1 (&&) $ 
                    map ($m) [
                              blockValid
                             ,horizontalValid
                             ,verticalValid
                             ]
    where blockValid      = all not.map hasSingeltonCopies.map2d (replicate 1).concat.concatBlock.excludeNonSingelton
          horizontalValid = all not.map hasSingeltonCopies
          verticalValid   = horizontalValid.transpose 

solve :: Sudoku [Int] -> [Sudoku [Int]] --returns solved sudokus 
solve m
    | solved rm = [rm]
    | (not.valid) rm = [] 
    | otherwise     = possibleChoices $ rm
    where possibleChoices = filter validityCheck.concat.map solve.fixationsSmallest
          rm = filterNonCollision m

solutions :: Sudoku Int -> [Sudoku Int] 
--wrapper to solve to maintain a [Int] in the solve
solutions = postprocess.solve.filterNonCollision.hypothesesSpace
    where postprocess = map certain

parse :: [String] -> [Sudoku Int]
parse [] = []
parse (_:a:b:c:d:e:f:g:h:i:rest) 
    = (map parseLine $ a:b:c:d:e:f:g:h:i:[]) : parse rest
    where parseLine = map (\d->read [d]::Int) 

magicNumber :: Sudoku Int -> Int
magicNumber m = convert3.choosen3 $ m ::Int
    where choosen3 = take 3.head
          convert3 = read.concat.(map show)

main = do
    contents <- getContents
    print.operation.parse.lines.cleanInput $ contents
        where cleanInput = filter (/='\r')
              operation = sum.parMap rdeepseq (magicNumber.head.solutions)

