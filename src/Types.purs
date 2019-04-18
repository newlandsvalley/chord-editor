module Types where

-- | a finger position on a string
-- | n < 0  : String is silent
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FingerPosition = Int

-- | a fretboard is represented as an array of finger positions
type Fretboard = Array FingerPosition

data ExportFormat =
    PNG
  | JPG

toMimeType :: ExportFormat -> String
toMimeType PNG = "image/png;base64"
toMimeType JPG = "image/jpeg"
