import Data.List

test_suduko :: Suduko
test_suduko = [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]

takenh = (take 9).repeat -- The zero is just ignored later
takenv = transpose.takenh.transpose --to ol' `transform to a known problem, solve it and then transform it back`
--takenb = groupby 3 rows --taken in the box

group3by3 = transpose.group3.transpose.group3 --group the actual suduko returns a 3x3x9 tensor
group3 = iterate somehow ?--group in one direction 

data Entry = 0 | 1 | ... | 9
type Suduko = [[Entry]]
type Suduko_Memory = [[[Entry]]] --a list foreach tile to remember, ex. possabilityies left 
omega = [1..9]

numbers_left:: Suduko ->
numbers_left suduko =  omega \\ foldl1 union $ map ($ suduko) [takenh, takenv]

main = print.numbers_left $ test_suduko
