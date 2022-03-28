sum_ :: [Integer] -> Integer
sum_ = foldl (+) 0

prod_ :: [Integer] -> Integer
prod_ = foldl (*) 1

g :: [Integer] -> [Integer] 
g = foldr (:) []

