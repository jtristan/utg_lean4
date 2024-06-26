import «UtgLean4»

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

def explicitRanges (ucd : List UnicodeData) (property : UnicodeData → Bool) : StateM ((Option Range) × List Range) (List Range) := do
  for datapoint in ucd do
    let (rangeOpt, ranges) ← get
    let code := datapoint.codepoint
    let prop := property datapoint
    match rangeOpt, prop with
    | some r, true =>
      if r.stop + 1 = code then
        -- Extend the range
        set (some ({r with stop := code})  , ranges)
      else
        -- Hidden gap
        let completedRange : Range := { start := r.start , stop := r.stop + 1 }
        let newRange : Range := { start := code , stop := code }
        set (some newRange , completedRange :: ranges)
    | some r, false =>
      -- Close the range
      -- Cannot use code for range end as their may be a jump in codepoints
      let completedRange : Range := { start := r.start , stop := r.stop + 1 }
      set ((none : Option Range) , completedRange :: ranges)
    | none, true =>
      -- Open a range
      let newRange : Range := { start := code , stop := code }
      set (some newRange , ranges)
    | none, false =>
      -- Nothing interesting, moving on
      set (rangeOpt , ranges)

  return (← get).2

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

def indices (gaps : List Nat) : StateM (Nat × List Nat) (List Nat) := do
  for gap in gaps do
    let (index, indices) ← get
    if gap ≥ 256 then
      set (index + 1 , ((index + 1) * 2^21) :: indices)
    else
      set (index + 1, indices)
  return (← get).2.reverse

def prefixSums (gaps : List Nat) : StateM (Nat × List Nat) (List Nat) := do
  for gap in gaps do
    let (prefixSum, prefixSums) ← get
    if gap ≥ 256 then
      set (prefixSum + gap , (prefixSum + gap) :: prefixSums)
    else
      set (prefixSum + gap, prefixSums)
  return (← get).2.reverse

def largeOffsetEncoding (indices prefixSums : List Nat) : Array Nat :=
  let prefixSums := prefixSums ++ [1114111 + 1]
  ((indices.zip prefixSums).map (fun (idx,pf) => idx + pf)).toArray

structure UcdPropertyTable where
  runs : Array Nat
  offsets : Array UInt8
  deriving Repr, DecidableEq, Inhabited, Nonempty

instance : ToString UcdPropertyTable where
  toString := fun table => s!"runs:\n{table.runs}\noffsets:\n{table.offsets}"

def calculateTable (ucd : List UnicodeData) (property : UnicodeData → Bool) : UcdPropertyTable :=
  let (ranges,_,_) := (explicitRanges ucd property) |>.run (none,[])
  let gaps := mergeRanges ranges
  let offsets := offsets gaps
  let (indices, _) := indices gaps |>.run (0,[0])
  let (prefixSums, _) := prefixSums gaps |>.run (0,[])
  --dbg_trace s!"Indices: {indices}"
  --dbg_trace s!"Prefix sum: {prefixSums}"
  let runs := largeOffsetEncoding indices prefixSums
  { runs, offsets }

def searchRuns (table : UcdPropertyTable) (c : Char) : Nat × Range := Id.run do
  let codepoint := c.toNat
  let mut i := 0
  for run in table.runs do
    let prefixSum := run % 2^21
    --dbg_trace s!"Iteration: {i} {prefixSum}"
    if codepoint < prefixSum then -- careful > or ≥
      break
    i := i + 1
  let idx := i
  --dbg_trace s!"Idx: {idx} {codepoint} {table.runs.get! idx % 2^21}"
  let codepointStart := if idx = 0 then 0 else table.runs.get! (idx - 1) % 2^21
  let rangeStart := table.runs.get! idx / 2^21
  let rangeStop := if idx + 1 = table.runs.size then table.offsets.size else table.runs.get! (idx + 1) / 2^21
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

instance : ToString Range where
  toString := fun range : Range => s!"[{range.start}..{range.stop})"

def search (table : UcdPropertyTable) (c : Char) : Bool :=
  let (pfs,range) := searchRuns table c
  --dbg_trace s!"{pfs} {range}"
  let b := searchOffsets table c range pfs
  --dbg_trace s!"Parity: {b}"
  b

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

def compareTables (ucd : List UnicodeData) (property : UnicodeData → Bool) : Bool := Id.run do
  let table := calculateTable ucd property
  let referenceTable := referenceTable ucd property
  for i in Range.mk 0 1114112 1 do
    let c := Char.ofNat i
    let ref := referenceSearch referenceTable c
    let candidate := search table c
    if ref ≠ candidate then
      return false
  return true

-- theorem foo (ucd : List UnicodeData) (property : UnicodeData → Bool) (h : compareTables ucd property = true) (c : Char)
--     : skiplist ucd property c = reference ucd property c := by
--   simp
--   simp [compareTables] at h
--   have A : ∀ c : Char, c.toNat < 1114112 := by
--     intro c
--     have B := c.valid
--     simp [UInt32.isValidChar, Nat.isValidChar] at B
--     simp [Char.toNat]
--     rcases B with ⟨ h2 ⟩
--     . rename_i h2

--     . rename_i h2
--       rcases h2 with ⟨ h2, h4 ⟩
--       trivial



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
      -- printUnicodeData ucd₅
      println s! "UCD size: {ucd₅.length}"
      let summary := summarizeUnicodeData ucd₅
      printSummary summary
      let property := (fun ucdc : UnicodeData => if let .Number _ := ucdc.gc then true else false)
      let table := calculateTable ucd₅ property
      let referenceTable := referenceTable ucd₅ property
      println table
      let bound := 1200000
      for i in Range.mk 0 bound 1 do
        let c := Char.ofNat i
        let ref := referenceSearch referenceTable c
        let candidate := search table c
        if ref ≠ candidate then
          println s!"{c.toNat} {c} {ref} {candidate}"
      println s!"Size: {referenceTable.length * 4}"
      -- let time ← monoNanosNow
      -- for i in Range.mk 0 bound 1 do
      --   let c := Char.ofNat i
      --   let _ := referenceSearch referenceTable c
      -- println <| (← monoNanosNow) - time
      -- println s!"Size: {table.offsets.size + table.runs.size * 4}"
      -- let time ← monoNanosNow
      -- for i in Range.mk 0 bound 1 do
      --   let c := Char.ofNat i
      --   let _ := search table c
      -- println <| (← monoNanosNow) - time
      -- for i in Range.mk 0 100 1 do
      --   let rb ← getRandomBytes 1
      --   println rb

  | Except.error msg => println msg

-- If my function is going to initialize the state, do I still need to run?
-- Does the state needs to be returned?
-- If I declared variables as let mul, could I infer the state?

#check getRandomBytes
#check monoMsNow
