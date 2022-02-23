module Syntax where
import qualified Data.Map as Map

data Operator
  = Plus
  | Minus
  | Mal
  | Gleich
  deriving Show

data Zahl
  = Null 
  | Eis 
  | Zwei 
  | Drue 
  | Vier 
  | Foif 
  | Sechs 
  | Siebe 
  | Acht 
  | Nuen 
  | Zaeh 
  | Elf 
  | Zwoelf 
  | Drizaeh 
  | Vierzaeh 
  | Fuefzaeh 
  | Sechszaeh 
  | Siebezaeh 
  | Achtzaeh 
  | Nünzaeh 
  | Zwaenzg 
  | Driisg 
  | Vierzg 
  | Fuefzg 
  | Sechzg 
  | Siebezg 
  | Achzg 
  | Nuenzg 
  | Hundert 
  | Tuusig 
  | Eh
  deriving (Show, Eq, Ord)

type LookupTableEntry = Map.Map Zahl Integer

data RechnungsElemente = Zahl Zahl | Operator Operator

type Eingabe = [RechnungsElemente]