module Syntax where
import qualified Data.Map as Map

data Operator
  = Plus
  | Minus
  | Mal
  | Gleich
  deriving Show

type LookupTableEntry = Map.Map String Double

data RechnungsElemente = Zahl String | Operator Operator

type Eingabe = [RechnungsElemente]