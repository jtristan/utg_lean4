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
  else if s = "Ll" then pure <| GeneralCategory.Letter Letter.Ll
  else if s = "Lt" then pure <| GeneralCategory.Letter Letter.Lt
  else if s = "Lm" then pure <| GeneralCategory.Letter Letter.Lm
  else if s = "Lo" then pure <| GeneralCategory.Letter Letter.Lo
  else if s = "Mn" then pure <| GeneralCategory.Mark Mark.Mn
  else if s = "Mc" then pure <| GeneralCategory.Mark Mark.Mc
  else if s = "Me" then pure <| GeneralCategory.Mark Mark.Me
  else if s = "Nd" then pure <| GeneralCategory.Number Number.Nd
  else if s = "Nl" then pure <| GeneralCategory.Number Number.Nl
  else if s = "No" then pure <| GeneralCategory.Number Number.No
  else if s = "Pc" then pure <| GeneralCategory.Punctuation Punctuation.Pc
  else if s = "Pd" then pure <| GeneralCategory.Punctuation Punctuation.Pd
  else if s = "Ps" then pure <| GeneralCategory.Punctuation Punctuation.Ps
  else if s = "Pe" then pure <| GeneralCategory.Punctuation Punctuation.Pe
  else if s = "Pi" then pure <| GeneralCategory.Punctuation Punctuation.Pi
  else if s = "Pf" then pure <| GeneralCategory.Punctuation Punctuation.Pf
  else if s = "Po" then pure <| GeneralCategory.Punctuation Punctuation.Po
  else if s = "Sm" then pure <| GeneralCategory.Symbol Symbol.Sm
  else if s = "Sc" then pure <| GeneralCategory.Symbol Symbol.Sc
  else if s = "Sk" then pure <| GeneralCategory.Symbol Symbol.Sk
  else if s = "So" then pure <| GeneralCategory.Symbol Symbol.So
  else if s = "Zs" then pure <| GeneralCategory.Separator Separator.Zs
  else if s = "Zl" then pure <| GeneralCategory.Separator Separator.Zl
  else if s = "Zp" then pure <| GeneralCategory.Separator Separator.Zp
  else if s = "Cc" then pure <| GeneralCategory.Other Other.Cc
  else if s = "Cf" then pure <| GeneralCategory.Other Other.Cf
  else if s = "Cs" then pure <| GeneralCategory.Other Other.Cs
  else if s = "Co" then pure <| GeneralCategory.Other Other.Co
  else if s = "Cn" then pure <| GeneralCategory.Other Other.Cn
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

def loadUnicodeData (file : FilePath) : ExceptT String (StateT (List UnicodeData) IO) (List UnicodeData) := do
  let content : String ← readFile file
  let content : List String := content.splitOn "\n"
  for line in content do
    if line ≠ "" then -- UnicodeData.txt ends with an empty line
      let line : List String := line.splitOn ";"
      let codepoint : String := line.get! 0
      let gc : GeneralCategory ← mkGeneralCategory (line.get! 2)
      let data : List UnicodeData ← get
      match codepoint.toNatHex? with
      | none => throw "Conversion of codepoint failed"
      | some c => set <| { codepointRaw := codepoint , codepoint := c, gc := gc } :: data
  return (← get).reverse

structure summaryUCD where
  letterCount : Int := 0
  markCount : Int := 0
  numberCount : Int := 0
  punctuationCount : Int := 0
  symbolCount : Int := 0
  separatorCount : Int := 0
  otherCount : Int := 0
  deriving Repr, DecidableEq, Inhabited, Nonempty

def summarizeUnicodeData (ucd : List UnicodeData) : StateM summaryUCD summaryUCD := do
  for entry in ucd do
    let table ← get
    match entry.gc with
    | GeneralCategory.Letter _ => set { table with letterCount := table.letterCount + 1 }
    | GeneralCategory.Mark _ => set { table with markCount := table.markCount + 1 }
    | GeneralCategory.Number _ => set { table with numberCount := table.numberCount + 1 }
    | GeneralCategory.Punctuation _ => set { table with punctuationCount := table.punctuationCount + 1 }
    | GeneralCategory.Symbol _ => set { table with symbolCount := table.symbolCount + 1 }
    | GeneralCategory.Separator _ => set { table with separatorCount := table.separatorCount + 1 }
    | GeneralCategory.Other _ => set { table with otherCount := table.otherCount + 1 }
  return ← get

def printSummary (sucd : summaryUCD) : IO Unit := do
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

def gapEncoding (ucd : List UnicodeData) (property : UnicodeData → Bool) : StateM (Bool × Nat × Nat × List Nat) (List Nat) := do
  for datapoint in ucd do
    let (inside, offset, last, ranges) ← get
    let prop := property datapoint
    let code := datapoint.codepoint
    if xor inside prop then
      let delta := if inside then 1 + last - offset else code - offset
      let newOffset := if inside then 1 + last else code
      dbg_trace "{reprStr datapoint}"
      if delta < 256 then
        set (! inside, newOffset, code, delta :: ranges)
        dbg_trace "{code} {delta}"
      else
        set (! inside, newOffset, code, 0 :: ranges)
        dbg_trace "{code} {0}"
    else if inside && code ≠ last + 1 then -- jump
      let delta₁ := 1 + last - offset
      let delta₁ := if delta₁ < 256 then delta₁ else 0
      let delta₂ := code - (last + 1)
      let delta₂ := if delta₂ < 256 then delta₂ else 0
      set (inside, code, code, delta₂ :: delta₁ :: ranges)
    else
      set (inside, offset, code, ranges)
  let (_, _, _, ranges) ← get
  return ranges.reverse

def filter (ucdc : UnicodeData) : Bool :=
  match ucdc.gc with
  | GeneralCategory.Number Number.Nd => true
  | GeneralCategory.Number Number.Nl => true
  | GeneralCategory.Number Number.No => true
  | _ => false

def main : IO Unit := do
  let workingDir : FilePath ← currentDir
  let dataDir : FilePath := join workingDir (System.mkFilePath ["Data"])
  if (¬ (← dataDir.pathExists)) then
    createDir "Data"
  for dataset in unicodeDatasets do
    let f : FilePath := System.mkFilePath ["Data",dataset]
    let _ ← download (unicodeUrl ++ dataset) f

  let f : FilePath := System.mkFilePath ["Data","UnicodeData.txt"]
  let ucd₁ : ExceptT String (StateT (List UnicodeData) IO) (List UnicodeData) := loadUnicodeData f
  let ucd₂ : IO (Except String (List UnicodeData) × List UnicodeData) := ucd₁ |>.run []
  let ucd₃ : (Except String (List UnicodeData)) × List UnicodeData ← ucd₂
  let ucd₄ : Except String (List UnicodeData) := ucd₃.1
  match ucd₄ with
  | Except.ok ucd₅ =>
      -- printUnicodeData ucd₅
      println s! "UCD size: {ucd₅.length}"
      let summary := summarizeUnicodeData ucd₅ |>.run {}
      printSummary summary.1
      -- state is returned and comes second
      let (gaps,_,_) := gapEncoding ucd₅ filter |>.run (false, 0, 0, [])
      println gaps
  | Except.error msg => println msg
