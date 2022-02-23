module Semantics where
import Syntax
import qualified Data.Map as Map

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
    where
        fetchValue :: Zahl -> Integer
        fetchValue k = fromJust (Map.lookup k makeZahlList)

        plus :: Zahl -> Zahl -> Integer
        plus a b = fetchValue a + fetchValue b
        
        minus :: Zahl -> Zahl -> Integer
        minus a b = fetchValue a - fetchValue b

        mal :: Zahl -> Zahl -> Integer
        mal a b = fetchValue a * fetchValue b