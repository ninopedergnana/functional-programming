
{- Syntax
    Implementieren Sie den AST (abstract syntax tree) entsprechend den Vorgaben
    auf dem Übungsblatt.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
data Regex
    = Empty
    | Epsilon
    | Symbol Char
    | Sequence Regex Regex
    | Star Regex
    | Choice Regex Regex
    -- | Sequence, Star, Or fehlen noch.
    deriving Show

{- Semantik
    Implementieren Sie eine Funktion, die folgende Spezifikation erfüllt:
      "We want a function that given a regex computes a list of all strings matching
       this regex. However, if the regex contains the 'Star' constructor, then the
       resulting list will be infinite. Therefore, we add a counter limiting the
       number of repetitions allowed by the star operator. When checking if a string
       matches the regex, we can use the length of the string as limit."
-}
generate :: Int -> Regex -> [String]
generate limit r = case r of
    Empty -> []
    Epsilon -> [""]
    Symbol c -> [[c]]
    Choice r s -> generate limit r ++ generate limit s
    Sequence r s -> [x++y | x <- generate limit r, y <- generate limit s]
    Star r -> error "fixme"

{-
    Matching a particular string to a regex a regex now simply means to check
    whether the string is an element of the list of all matching strings.
    The string's length is a rough upper bound on the number of repetitions
    for any application of the 'Star' operator.
-}
match :: Regex -> String -> Bool
match r s = s `elem` generate (length s) r

{-
    Examples/test-cases
-}

-- r = a(a|b)*
r :: Regex
r = Sequence (Symbol 'a') $ Star $ Choice (Symbol 'a') (Symbol 'b')

shouldBeTrue_r :: Bool
shouldBeTrue_r = match r "abababa"

shouldBeFalse_r :: Bool
shouldBeFalse_r = match r ""

-- s = x*a*b*
s :: Regex
s = Sequence (Star (Symbol 'x')) $ Sequence (Star (Symbol 'a')) (Star (Symbol 'b'))

shouldBeTrue_s :: Bool
shouldBeTrue_s = match s "xxxxxaaab"

shouldBeFalse_s :: Bool
shouldBeFalse_s = match r "aabbx"