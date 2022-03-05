-----------------------------------------------------------
-- Referential transparency
-----------------------------------------------------------

{- Referential Transparency
    It is always possible to exchange an expression with
    its definition.
-}
myThree :: Integer
myThree = if True then 3 else 4

myNumber :: Integer
myNumber = myThree + 7

anotherWayToPutIt :: Integer
anotherWayToPutIt =
    (if True then 3 else 4) + 7
--  ^--this is 'myThree'--^

{- Imperative Code
    Imperative Code is not usually referrentially
    transparent.

    x = 3;
    x = x + 1;
    print x + 4

    vs

    x = 3;
    x = x + 1;
    print 3 + 4

-}

-----------------------------------------------------------
-- Sum Types
-----------------------------------------------------------

-- Playing Cards

-- King, Queen, etc.
data Face = Ace | King | Queen | Jack | Number Int
    deriving (Show, Eq)

-- Heart, Spade, etc.
data Suit = Spade | Diamond | Heart | Club
    deriving (Show, Eq)

-- A card consists of a face and a suit
data Card = Card {
    face :: Face,
    suit :: Suit
    } deriving (Show, Eq)

-- A hand holds multiple cards
type Hand = [Card]


{- Exercise
    Write a function 'evalCard' that returns all possible
    values of a single card as follows:
    * A card with a number has the value of the number
    * An Ace has two possible values: 1 and 11
    * All other cards are valued 10.
-}
evalCard :: Card -> [Int]
evalCard c = case face c of
  Ace -> [1,11]
  King -> [10]
  Queen -> [10]
  Jack -> [10]
  Number n -> [n]
-- evalCard Card {face = King, suit = Heart}

{- Exercise
    Write a function 'evalHand' that returns all possible
    values of a hand (use foldl). Dublicates are OK.
-}
evalHand :: Hand -> [Int]
evalHand h = foldl f [0] h
    where
        f zeroList hand = [ x+y | x <- zeroList, y <- evalCard hand]
-- foldl takes the second argument [0] (empty list, but mustn't be empty) and applies the function and the list to it.



{- Exercise
    Specify two hands:
    hand1 = 🂡,🂥,🃓,🃞
    hand2 = 🃍,🃁,🂡,🂪,🂧
-}

hand1 :: Hand
hand1 = [Card {face = Jack, suit = Heart}, Card {face = Ace, suit = Spade}, Card {face = Number 2, suit = Heart}]

hand2 :: Hand
hand2 = [Card {face = Ace, suit = Heart}, Card {face = Ace, suit = Spade}, Card {face = Number 4, suit = Heart}]

{- Exercise
    Test 'evalHand' with 'hand1' and 'hand2' using the repl.
-}

-- Shapes

-- A shape is either a rectangle, a square, or a circle
data Shape
    = Rectangle Float Float
    | Square Float
    | Circle Float
    deriving (Show, Eq)

-- Calculates the area of a shape
area :: Shape -> Float
area (Rectangle a b) = a * b
area (Square a) = a * a
area (Circle r) = pi * r * r

{- Exercise
    Write a function to calculate the circumference of a shape.
    Use the 'case .. of' syntax for pattern matching.
-}
circ :: Shape -> Float
circ shape = case shape of
    Rectangle a b -> (a + b) * 2
    Square a -> a * 4
    Circle a -> 2 * a * pi

-----------------------------------------------------------
-- Typeclasses
-----------------------------------------------------------

{- Monoid
   There is a typeclass called Monoid that features
   1. One special (neutral) element 'e'
   2. A binary operation 'op'
   Moreover, there are rules:
   1. e `op` x = x `op` e = x, for all x
   2. (x `op` y) `op`z = x `op` (y `op`z) for all x,y,z

   **Note**
   The "official" names in Haskell for 'e' and 'op' are
   'mempty' and 'mconcat'.
-}
class MyMonoid a where
    e :: a
    op :: a -> a -> a

instance MyMonoid Int where
    e = 0
    op = (+)

{- Exercise
    Write a Monoid instance for [a] that satisfies the rules.
-}

instance MyMonoid [a] where
    e = error "fixme"
    op = error "fixme"