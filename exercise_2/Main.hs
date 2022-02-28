import Data.List

newtype Horsepower = HorsePower String
        deriving (Show, Eq, Ord)
newtype Model = Model String
        deriving (Show, Eq, Ord)
newtype Color = Color String
        deriving (Show, Eq, Ord)


data Car = Car
        { model :: Model
        , year :: Integer
        , color :: Color
        , power :: Horsepower
        } deriving (Show, Eq)

ford :: Car
ford = Car (Model "Fiesta") 1996 (Color "Green") (HorsePower "80PS")

ferrari :: Car
ferrari = Car (Model "Ferrari") 2002 (Color "Blue") (HorsePower "60PS")

renault :: Car
renault = Car (Model "Renault") 2009 (Color "Red") (HorsePower "100PS")

instance Ord Car where
        compare a b = compare
                (power a, year a, model a, color a)
                (power a, year a, model a, color a)

unsortedList :: [Car]
unsortedList = [ford, renault, ferrari]

sortedList :: [Car]
sortedList = sort [renault, ford, ferrari]


data Tree a =
        Node (Tree a) a (Tree a)
        | Leaf a
        | Nil
        deriving (Show, Eq, Ord)

collect :: Tree a -> [a]
collect tree = case tree of
        Nil -> undefined
        Leaf a -> [a]
        Node left a right -> [a] ++ collect left ++ collect right


data Tree' a = Tree' a [Tree' a]
        deriving (Show, Eq, Ord)

baum :: Tree' Int
baum = Tree' 1 
	[Tree' 2 
	[Tree' 5 [], Tree' 6 [], Tree' 7 []]
	, Tree' 3 
	[Tree' 8 []]
	]


