import UtgLean4.Init.Lookup
import UtgLean4.Init.Tables

def isNumeric (c : Char) : Bool :=
  search numericTable c
