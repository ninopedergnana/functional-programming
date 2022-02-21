module Semantics where
import Syntax
import qualified Data.Map as Map


yo :: [Integer]
yo = [1, 2, 3, 4, 5]

ya :: [Zahl]
ya = [Eis, Zwei, Drü, Vier, Foif]

makeZahlList :: LookupTableEntry
makeZahlList = Map.fromList (zip ya yo)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

fetchValue :: Zahl -> Integer
fetchValue k = fromJust (Map.lookup k makeZahlList)


