import Data.List (sort)

-- [[1,2,3],[1,3,2], [1]] -> [1,2,3,1,3,2,1]

-- Aufgabe 1
flatSort :: [[Integer]] -> [Integer]
flatSort = concat

-- Aufgabe 2 


-- Aufgabe 3
flatSortRecursion :: [[Integer]] -> [Integer]
flatSortRecursion [] = []
flatSortRecursion [x] = x
flatSortRecursion (x:xs) = x ++ flatSortRecursion xs

-- Aufgabe 4
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

