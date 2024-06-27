

open Std

structure UcdPropertyTable where
  runs : Array UInt32
  offsets : Array UInt8
  deriving Repr, DecidableEq, Inhabited, Nonempty

instance : ToString UcdPropertyTable where
  toString := fun table => s!"runs:\n{table.runs}\noffsets:\n{table.offsets}"

def searchRuns (table : UcdPropertyTable) (c : Char) : Nat × Range := Id.run do
  let codepoint := c.toNat
  let mut i := 0
  for run in table.runs do
    let prefixSum := run.toNat % 2^21
    if codepoint < prefixSum then
      break
    i := i + 1
  let idx := i
  let codepointStart := if idx = 0 then 0 else (table.runs.get! (idx - 1)).toNat % 2^21
  let rangeStart := (table.runs.get! idx).toNat / 2^21
  let rangeStop := if idx + 1 = table.runs.size then table.offsets.size else (table.runs.get! (idx + 1)).toNat / 2^21
  let range : Range := Range.mk rangeStart rangeStop 1
  return (codepointStart, range)

def searchOffsets (table : UcdPropertyTable) (c : Char) (range : Range) (pfs : Nat) : Bool := Id.run do
  let codepoint := c.toNat
  let mut i := 0
  let mut prefixSum := pfs
  for j in range do
    if codepoint < prefixSum + (table.offsets.get! j).toNat then
      i := j
      break
    else
      prefixSum := prefixSum + (table.offsets.get! j).toNat
  return i % 2 = 1

def search (table : UcdPropertyTable) (c : Char) : Bool :=
  let (pfs,range) := searchRuns table c
  searchOffsets table c range pfs
