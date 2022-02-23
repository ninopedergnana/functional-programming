module Semantics where
import Syntax
import qualified Data.Map as Map
import Data.List.Split

yo :: [Integer]
yo = [1, 2, 3, 4, 5]

ya :: [Zahl]
ya = [Eis, Zwei, Drue, Vier, Foif]

makeZahlList :: LookupTableEntry
makeZahlList = Map.fromList (zip ya yo)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

run :: Eingabe -> Integer
run calc = case calc of
    [] -> 0
    Zahl Eis:_ -> fetchValue Eis
    Zahl Zwei:_ -> fetchValue Zwei
    Zahl Drue:_ -> fetchValue Drue
    Zahl Vier:_ -> fetchValue Vier
    Zahl Foif:_ -> fetchValue Foif
    Zahl a:Operator Plus:Zahl b:_ -> plus a b
    Zahl a:Operator Minus:Zahl b:_ -> minus a b
    Zahl a:Operator Mal:Zahl b:_ -> mal a b
    where
        fetchValue :: Zahl -> Integer
        fetchValue k = fromJust (Map.lookup k makeZahlList)

        plus :: Zahl -> Zahl -> Integer
        plus a b = fetchValue a + fetchValue b
        
        minus :: Zahl -> Zahl -> Integer
        minus a b = fetchValue a - fetchValue b

        mal :: Zahl -> Zahl -> Integer
        mal a b = fetchValue a * fetchValue b


execute :: String -> Integer
execute str = run splitOn " " str