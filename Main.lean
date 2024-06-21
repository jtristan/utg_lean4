import «UtgLean4»

open System IO FilePath Process FS

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
  else throw "ddl"

abbrev CombiningClass := UInt8

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
  codepoint : Int32
  name : String
  gc : GeneralCategory
  cc : CombiningClass

def loadUnicodeData (file : FilePath) : ExceptT String IO Unit := do
  let content : String ← readFile file
  let content : List String := content.splitOn "\n"
  for line in content do
    if line ≠ "" then -- UnicodeData.txt ends with an empty line
      --println line
      let line : List String := line.splitOn ";"
      let codepoint : String := line.get! 0
      let name : String := line.get! 1
      let gc : GeneralCategory ← mkGeneralCategory (line.get! 2)
      println <| reprStr gc

def main : IO Unit := do
  let workingDir : FilePath ← currentDir
  let dataDir : FilePath := join workingDir (System.mkFilePath ["Data"])
  if (¬ (← dataDir.pathExists)) then
    createDir "Data"
  for dataset in unicodeDatasets do
    let f : FilePath := System.mkFilePath ["Data",dataset]
    let _ ← download (unicodeUrl ++ dataset) f

  let f : FilePath := System.mkFilePath ["Data","UnicodeData.txt"]
  try
    let _ ← loadUnicodeData f
  catch msg =>
    println msg
