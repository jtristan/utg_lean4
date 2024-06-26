
open Std

/-- Is the character in `0123456789ABCDEF`? -/
def Char.isHexDigit (c : Char) : Bool :=
  c.val ≥ 48 && c.val ≤ 57 || c.val ≥ 65 && c.val ≤ 70

def String.isNatHex (s : String) : Bool :=
  !s.isEmpty && s.all (·.isHexDigit)

def String.toNatHex? (s : String) : Option Nat :=
  if s.isNatHex then
    some <| s.foldl (fun n c =>  n*16 + (if c.isDigit then c.toNat - '0'.toNat else 10 + (c.toNat - 'A'.toNat))) 0
  else
    none

def String.toNatHex! (s : String) : Nat :=
  if s.isNatHex then
    s.foldl (fun n c =>  n*16 + (if c.isDigit then c.toNat - '0'.toNat else 10 + (c.toNat - 'A'.toNat))) 0
  else
    panic! "Nat in hexadecimal expected"

instance : ToString Range where
  toString := fun range : Range => s!"[{range.start}..{range.stop})"
