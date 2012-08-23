
positions = [10^i|i<-[0..6]]

-- 0.1234567891011121314151617181920212223242526272829303132333435363738394041424344454647484950
--
-- 1 2 3 4 5 6 7 8 9  #cell1
-- 
-- 10 11 12 13 14 15 16 17 18 19 #cell2
-- 20 21 22 23 24 25 26 27 28 29
-- .............................
-- 90 91 92 93 94 95 96 97 98 99
--
-- 100 101 102 103 104 105 106 107 108 109 #cell3
-- 110 111 112 113 114 115 116 117 118 119
-- .......................................
-- 990 991 992 993 994 995 996 997 998 999
--
-- 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 #cell4
-- 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019
-- .................................................
-- 9990 9991 9992 9993 9994 9995 9996 9997 9998 9999
-- 
--
-- 9990 block (which of the different blocks in cell)
-- 
-- 9990
-- |_alignment 
--
-- alignment = mod offset block
-- block     = div offset block

cell_size::Integer->Integer
cell_size 1 = 9
cell_size n = 10*n*(10^(n-1)-1) --number of digits in cell

get_base_pos::Integer->Integer
get_base_pos 1 = 1 --starts at one
get_base_pos n = sum . ( map cell_size ) $ [1..(n-1)] --get the start position of the cell

get_cell::Integer->Integer --TESTED
get_cell 1 = 1
get_cell n =  last . (takeWhile ((<n).(get_base_pos))) $ [1..] --gets the cell in which the digit resides

main = print . (map get_cell ) $ positions

