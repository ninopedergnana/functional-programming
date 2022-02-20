import Data.List (sort)

-- Aufgabe 1
-- flatSort [[1,2,3],[1,3,2], [1]] -> [1,2,3,1,3,2,1]
flatSort :: [[Integer]] -> [Integer]
flatSort xs = concat $ map sort xs

-- Aufgabe 2 
-- concat [[3,2,1],[4]] -> [3,2,1,4]
-- flatSort [[3,2,1], [4]] -> [1,2,3,4]

-- Aufgabe 3
-- flatSortRecursion [[1,2,3],[1,3,2], [1]] -> [1,2,3,1,3,2,1]
flatSortRecursion :: [[Integer]] -> [Integer]
flatSortRecursion [] = []
flatSortRecursion [x] = x
flatSortRecursion (x:xs) = x ++ flatSortRecursion xs

-- Aufgabe 4
-- initial "abc" "abcd" -> True
-- initial "abc" "asdfabc" -> False
initial :: String -> String -> Bool
initial x y = 
    x == take z y
    where 
        z = length x

-- Aufgabe 5
-- subString "abc" "bbabcd" == True
-- subString "abc" "bcd" == False
subString :: String -> String -> Bool
subString x [] = False
subString x y = 
    initial x y || subString x (tail y)

