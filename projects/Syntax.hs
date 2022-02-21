module Syntax where
import qualified Data.Map as Map

data Operator
  = Plus
  | Minus
  | Mal
  | Durch
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
  | Füfzäh 
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

data Rechnung
  = GanzeRechnung Zahl Operator Zahl
  | ZusatzRechnung Operator Zahl
  | NurZahl Zahl

type Eingabe = [Rechnung]