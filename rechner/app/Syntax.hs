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
  | Drü 
  | Vier 
  | Foif 
  | Sechs 
  | Siebe 
  | Acht 
  | Nün 
  | Zäh 
  | Elf 
  | Zwölf 
  | Drizäh 
  | Vierzäh 
  | Fuefzäh 
  | Sechszäh 
  | Siebezäh 
  | Achtzäh 
  | Nünzäh 
  | Zwänzg 
  | Driisg 
  | Vierzg 
  | Füfzg 
  | Sechzg 
  | Siebezg 
  | Achzg 
  | Nünzg 
  | Hundert 
  | Tuusig 
  | Eh
  deriving (Show, Eq, Ord)

type LookupTableEntry = Map.Map Zahl Integer

data RechnungsElemente = Zahl Zahl | Operator Operator

type Eingabe = [RechnungsElemente]