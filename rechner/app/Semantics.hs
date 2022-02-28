{-# LANGUAGE ScopedTypeVariables #-}
module Semantics where
import Syntax
import qualified Data.Map as Map

zahlenWerte :: [Double]
zahlenWerte = [1.0, 2.0, 3.0, 4.0, 5.0]

zahlenNamen :: [String]
zahlenNamen = ["eis", "zwei", "drü", "vier", "foif"]

makeZahlList :: LookupTableEntry
makeZahlList = Map.fromList (zip zahlenNamen zahlenWerte)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

fetchValue :: String -> Double
fetchValue k = fromJust (Map.lookup k makeZahlList)

rechne :: String -> [Double]
rechne = foldl calc [] . words

-- initial "abc" "abcd" -> True
-- initial "abc" "asdfabc" -> False
initial :: String -> String -> Bool
initial x y =
  x == take z y
  where
    z = length x

calc s x
        | x `elem` ["plus","minus","mal","durch","hoch"] = operate x s
        | otherwise = read x:s
        where
                operate op (x:y:s) = case op of
                        "plus" -> x + y:s
                        "minus" -> y - x:s
                        "mal" -> x * y:s
                        "durch" -> y / x:s
                        "hoch" -> y ** x:s