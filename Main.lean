import «UtgLean4»
import UtgLean4.Lookup
import UtgLean4.Util

open System IO FilePath Process FS Std

def unicodeUrl : String := "https://www.unicode.org/Public/UCD/latest/ucd/"
def unicodeDatasets : List String := ["UnicodeData.txt"]

def download  (url : String) (file : FilePath) : IO Output := do
  if (¬ (← file.pathExists)) then
    output { cmd := "curl", args := #["-s", "-S", "-f", "-o", file.toString, "-L", url] }
  else pure { exitCode := 0, stdout := "", stderr := "" }

inductive Letter where
  | Lu -- uppercase
  | Ll -- lowercase
  | Lt -- titlecase
  | Lm -- modifier
  | Lo -- other
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Mark where
  | Mn -- nonspacing
  | Mc -- spacing combining
  | Me -- enclosing
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Number where
  | Nd -- decimal digit
  | Nl -- letter
  | No -- other
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Punctuation where
  | Pc -- connector
  | Pd -- dash
  | Ps -- open
  | Pe -- close
  | Pi -- initial quote
  | Pf -- final quote
  | Po -- other
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Symbol where
  | Sm -- math
  | Sc -- currency
  | Sk -- modifier
  | So -- other
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Separator where
  | Zs -- space
  | Zl -- line
  | Zp -- paragraph
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive Other where
  | Cc -- control
  | Cf -- format
  | Cs -- surrogate
  | Co -- private use
  | Cn -- not assigned
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive GeneralCategory where
  | Letter (minor : Letter)
  | Mark (minor : Mark)
  | Number (minor : Number)
  | Punctuation (minor : Punctuation)
  | Symbol (minor : Symbol)
  | Separator (minor : Separator)
  | Other (minor : Other)
  deriving Repr, DecidableEq, Inhabited, Nonempty

inductive BidiClass where
  | AL  -- Arabic Letter
  | AN  -- Arabic Number
  | B   -- Paragraph Separator
  | BN  -- Boundary Neutral
  | CS  -- Common Separator
  | EN  -- European Number
  | ES  -- European Separator
  | ET  -- European Terminator
  | FSI -- First Strong Isolate
  | L   -- Left To Right
  | LRE -- Left To Right Embedding
  | LRI -- Left To Right Isolate
  | LRO -- Left To Right Override
  | NSM -- Nonspacing Mark
  | ON  -- Other Neutral
  | PDF -- Pop Directional Format
  | PDI -- Pop Directional Isolate
  | R   -- Right To Left
  | RLE -- Right To Left Embedding
  | RLI -- Right To Left Isolate
  | RLO -- Right To Left Override
  | S   -- Segment Separator
  | WS  -- White Space

structure UnicodeData where
  codepointRaw : String
  codepoint : Nat
  gc : GeneralCategory
  deriving Repr, DecidableEq, Inhabited, Nonempty

def mkGeneralCategory (s : String) : Except String GeneralCategory := do
  if s = "Lu" then pure <| GeneralCategory.Letter Letter.Lu
  else if s = "Ll" then pure <| .Letter .Ll
  else if s = "Lt" then pure <| .Letter .Lt
  else if s = "Lm" then pure <| .Letter .Lm
  else if s = "Lo" then pure <| .Letter .Lo
  else if s = "Mn" then pure <| .Mark .Mn
  else if s = "Mc" then pure <| .Mark .Mc
  else if s = "Me" then pure <| .Mark .Me
  else if s = "Nd" then pure <| .Number .Nd
  else if s = "Nl" then pure <| .Number .Nl
  else if s = "No" then pure <| .Number .No
  else if s = "Pc" then pure <| .Punctuation .Pc
  else if s = "Pd" then pure <| .Punctuation .Pd
  else if s = "Ps" then pure <| .Punctuation .Ps
  else if s = "Pe" then pure <| .Punctuation .Pe
  else if s = "Pi" then pure <| .Punctuation .Pi
  else if s = "Pf" then pure <| .Punctuation .Pf
  else if s = "Po" then pure <| .Punctuation .Po
  else if s = "Sm" then pure <| .Symbol .Sm
  else if s = "Sc" then pure <| .Symbol .Sc
  else if s = "Sk" then pure <| .Symbol .Sk
  else if s = "So" then pure <| .Symbol .So
  else if s = "Zs" then pure <| .Separator .Zs
  else if s = "Zl" then pure <| .Separator .Zl
  else if s = "Zp" then pure <| .Separator .Zp
  else if s = "Cc" then pure <| .Other .Cc
  else if s = "Cf" then pure <| .Other .Cf
  else if s = "Cs" then pure <| .Other .Cs
  else if s = "Co" then pure <| .Other .Co
  else if s = "Cn" then pure <| .Other .Cn
  else throw s!"Unknown General Category: {s}"


def loadUnicodeData (file : FilePath) : ExceptT String IO (List UnicodeData) := do
  let content : String ← readFile file
  let content : List String := content.splitOn "\n"
  let mut data := []
  for line in content do
    if line ≠ "" then -- UnicodeData.txt ends with an empty line
      let line : List String := line.splitOn ";"
      let codepoint : String := line.get! 0
      let gc : GeneralCategory ← mkGeneralCategory (line.get! 2)
      match codepoint.toNatHex? with
      | none => throw "Conversion of codepoint failed"
      | some c => data := { codepointRaw := codepoint , codepoint := c, gc := gc } :: data
  return data.reverse

structure SummaryUCD where
  letterCount : Int := 0
  markCount : Int := 0
  numberCount : Int := 0
  punctuationCount : Int := 0
  symbolCount : Int := 0
  separatorCount : Int := 0
  otherCount : Int := 0
  deriving Repr, DecidableEq, Inhabited, Nonempty

def summarizeUnicodeData (ucd : List UnicodeData) : SummaryUCD := Id.run do
  let mut table : SummaryUCD := {}
  for entry in ucd do
    match entry.gc with
    | GeneralCategory.Letter _ => table := { table with letterCount := table.letterCount + 1 }
    | GeneralCategory.Mark _ => table := { table with markCount := table.markCount + 1 }
    | GeneralCategory.Number _ => table := { table with numberCount := table.numberCount + 1 }
    | GeneralCategory.Punctuation _ => table := { table with punctuationCount := table.punctuationCount + 1 }
    | GeneralCategory.Symbol _ => table := { table with symbolCount := table.symbolCount + 1 }
    | GeneralCategory.Separator _ => table := { table with separatorCount := table.separatorCount + 1 }
    | GeneralCategory.Other _ => table := { table with otherCount := table.otherCount + 1 }
  return table

def printSummary (sucd : SummaryUCD) : IO Unit := do
  println s!"Letter count: {sucd.letterCount}"
  println s!"Mark count: {sucd.markCount}"
  println s!"Number count: {sucd.numberCount}"
  println s!"Punctuation count: {sucd.punctuationCount}"
  println s!"Symbol count: {sucd.symbolCount}"
  println s!"Separator count: {sucd.separatorCount}"
  println s!"Other count: {sucd.otherCount}"

def printUnicodeData (ucd : List UnicodeData) : IO Unit := do
  for entry in ucd do
    println <| reprStr entry

def explicitRanges (ucd : List UnicodeData) (property : UnicodeData → Bool) : List Range := Id.run do
  let mut rangeOpt : Option Range := none
  let mut ranges := []
  for datapoint in ucd do
    let code := datapoint.codepoint
    let prop := property datapoint
    match rangeOpt, prop with
    | some r, true =>
      if r.stop + 1 = code then
        -- Extend the range
        rangeOpt := some {r with stop := code}
      else
        -- Hidden gap
        let completedRange : Range := { start := r.start , stop := r.stop + 1 }
        let newRange : Range := { start := code , stop := code }
        rangeOpt := some newRange
        ranges := completedRange :: ranges
    | some r, false =>
      -- Close the range
      -- Cannot use code for range end as their may be a jump in codepoints
      let completedRange : Range := { start := r.start , stop := r.stop + 1 }
      rangeOpt := none
      ranges := completedRange :: ranges
    | none, true =>
      -- Open a range
      let newRange : Range := { start := code , stop := code }
      rangeOpt := some newRange
    | none, false => ()

  return ranges

def mergeRanges (ranges : List Range) : List Nat := Id.run do
  let flat := ranges.foldl (fun acc => fun range => range.start :: range.stop :: acc) []
  let mut prev := 0
  let mut gaps := []
  for bound in flat do
    gaps := (bound - prev) :: gaps
    prev := bound
  gaps := (0 :: gaps).reverse
  return gaps

def offsets (gaps : List Nat) : Array UInt8 :=
  (gaps.map (fun gap => if gap ≥ 256 then 0 else gap.toUInt8)).toArray

def indices (gaps : List Nat) : List Nat := Id.run do
  let mut index := 0
  let mut indices := [0]
  for gap in gaps do
    index := index + 1
    if gap ≥ 256 then
      indices := index * 2^21 :: indices
  return indices.reverse

def prefixSums (gaps : List Nat) : List Nat := Id.run do
  let mut prefixSum := 0
  let mut prefixSums := []
  for gap in gaps do
    prefixSum := prefixSum + gap
    if gap ≥ 256 then
      prefixSums := prefixSum :: prefixSums
  return prefixSums.reverse

def largeOffsetEncoding (indices prefixSums : List Nat) : Array UInt32 :=
  let prefixSums := prefixSums ++ [1114111 + 1]
  ((indices.zip prefixSums).map (fun (idx,pf) => (idx + pf).toUInt32)).toArray

def calculateTable (ucd : List UnicodeData) (property : UnicodeData → Bool) : UcdPropertyTable :=
  let ranges := (explicitRanges ucd property)
  let gaps := mergeRanges ranges
  let offsets := offsets gaps
  let indices := indices gaps
  let prefixSums := prefixSums gaps
  --dbg_trace s!"Indices: {indices}"
  --dbg_trace s!"Prefix sum: {prefixSums}"
  let runs := largeOffsetEncoding indices prefixSums
  { runs, offsets }

@[simp]
noncomputable def skiplist (ucd : List UnicodeData) (property : UnicodeData → Bool) (c : Char) :=
  let table := calculateTable ucd property
  search table c

def referenceTable (ucd : List UnicodeData) (property : UnicodeData → Bool) : List Nat :=
  (ucd.filter property).map (fun ucdc => ucdc.codepoint)

def referenceSearch (table : List Nat) (c : Char) : Bool :=
  table.contains c.toNat

@[simp]
noncomputable def reference (ucd : List UnicodeData) (property : UnicodeData → Bool) (c : Char) : Bool :=
  let table := referenceTable ucd property
  referenceSearch table c

def compareTables (ucd : List UnicodeData) (property : UnicodeData → Bool) : IO Unit := do
  let time ← monoMsNow
  let mut failed := false
  let table := calculateTable ucd property
  let referenceTable := referenceTable ucd property
  for i in Range.mk 0 1114112 1 do
    let c := Char.ofNat i
    let ref := referenceSearch referenceTable c
    let candidate := search table c
    if ref ≠ candidate then
      failed := true
      println s!"{c.toNat} {c} {ref} {candidate}"
  let msg := if failed then "failed" else "succeeded"
  println s!"Verification {msg} in {((← monoMsNow) - time).toFloat / 1000} seconds"

def writeTable : IO Unit := do
  let workingDir : FilePath ← currentDir
  let f : FilePath := join workingDir <| System.mkFilePath ["UtgLean4","Tables.lean"]
  let mut content := ""
  content := content ++ "import UtgLean4.Lookup\n"
  content := content ++ "\n"
  content := content ++ "instance numericTable : UcdPropertyTable where\n"
  content := content ++ "  runs := #[0]\n"
  content := content ++ "  offsets := #[0]\n"
  writeFile f content

def main : IO Unit := do
  let workingDir : FilePath ← currentDir
  let dataDir : FilePath := join workingDir (System.mkFilePath ["Data"])
  if (¬ (← dataDir.pathExists)) then
    createDir "Data"
  for dataset in unicodeDatasets do
    let f : FilePath := System.mkFilePath ["Data",dataset]
    let _ ← download (unicodeUrl ++ dataset) f

  let f : FilePath := System.mkFilePath ["Data","UnicodeData.txt"]
  let ucd₁ : ExceptT String IO (List UnicodeData) := loadUnicodeData f
  let ucd₄ : Except String (List UnicodeData) ← ucd₁
  match ucd₄ with
  | Except.ok ucd₅ =>
      println s! "UCD size: {ucd₅.length}"
      let summary := summarizeUnicodeData ucd₅
      printSummary summary
      let property := (fun ucdc : UnicodeData => if let .Number _ := ucdc.gc then true else false)
      compareTables ucd₅ property
      writeTable
  | Except.error msg => println msg

#check getRandomBytes
