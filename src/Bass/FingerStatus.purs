module Bass.FingerStatus (FingerStatus(..)) where

-- | a finger on a bass guitar pattern can be primary (black circle) or
-- | secondary (gray square) to indicate its importance

import Prelude (class Eq, class Show, (<<<), ($), const, pure)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Data.List.NonEmpty (singleton) as NEL
import Simple.JSON (class ReadForeign, class WriteForeign)
import Foreign (F, Foreign, ForeignError(..), readString, tagOf, unsafeToForeign)
import Control.Monad.Except (mapExcept)

data FingerStatus
  = Primary
  | Secondary

derive instance eqFingerStatus :: Eq FingerStatus

instance showFingerStatusInst :: Show FingerStatus where
  show = showFingerStatus

instance readFingerStatusInst :: ReadForeign FingerStatus where
  readImpl = readForeignFingerStatus

instance writeForeignFingerStatus :: WriteForeign FingerStatus where
  writeImpl = unsafeToForeign <<< showFingerStatus

readForeignFingerStatus :: Foreign -> F FingerStatus
readForeignFingerStatus value =
  mapExcept (either (const error) fromString) (readString value)
  where
  fromString = maybe error pure <<< readFingerStatus
  error = Left $ NEL.singleton $ TypeMismatch "FingerStatus" (tagOf value)

readFingerStatus :: String -> Maybe FingerStatus
readFingerStatus s =
  case s of
    "Primary" -> Just Primary
    "Secondary" -> Just Secondary
    _ -> Nothing

showFingerStatus :: FingerStatus -> String
showFingerStatus fs =
  case fs of
    Primary -> "Primary"
    Secondary -> "Secondary"
