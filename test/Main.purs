module Test.Main where

import Serialization.Json

import Bass.FingerStatus (FingerStatus(..))
import Bass.Types as Bass
import Bass.Validation (validate, validateJson) as BVAL
import Control.Monad.Free (Free)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (validation)
import Effect (Effect)
import FrettedInstrument.Types as FrettedInstrument
import FrettedInstrument.Validation (validate, validateJson) as FIVAL
import FrettedInstrument.Guitar.Config (config) as Guitar
import FrettedInstrument.TenorGuitar.Config (config) as TenorGuitar
import Piano.Types as Piano
import Piano.Validation (validate, validateJson) as PVAL
import Prelude (($), (<>), Unit, const, discard, negate)
import Test.Unit (TestF, suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  pianoSuite
  guitarSuite
  tenorGuitarSuite
  bassSuite

pianoSuite :: Free TestF Unit
pianoSuite =
  suite "piano serialization" do
    test "write F chord" do
      Assert.equal pianoFJSON $ writePiano pianoF
    test "read F chord" do
      validation (const $ failure "successful validation expected")
          (Assert.equal pianoF)
          (PVAL.validateJson pianoFJSON)
    test "read bad JSON" do
      validation (Assert.equal $ singleton "Not a recognisable piano chord format.")
          (const $ failure "bad JSON expected")
          (PVAL.validateJson badJSON)
    test "reject bad finger position" do
      validation (Assert.equal $ singleton "Finger position 24 is out of range.")
          (const $ failure "bad fingering expected")
          (PVAL.validate pianoBadFinger)

guitarSuite :: Free TestF Unit
guitarSuite =
  suite "guitar serialization" do
    test "write A chord" do
      Assert.equal guitarAJSON $ writeFrettedInstrument guitarA
    test "write F chord" do
      Assert.equal guitarFJSON $ writeFrettedInstrument guitarF
    test "read A chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal guitarA)
        (FIVAL.validateJson Guitar.config guitarAJSON)
    test "read F chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal guitarF)
        (FIVAL.validateJson Guitar.config guitarFJSON)
    test "read bad JSON" do
      validation (Assert.equal $ singleton "Not a recognisable guitar chord format.")
        (const $ failure "bad JSON expected")
        (FIVAL.validateJson Guitar.config badJSON)
    test "reject bad finger position" do
      validation (Assert.equal $ singleton "Finger position 50 is out of range.")
        (const $ failure "bad fingering expected")
        (FIVAL.validate Guitar.config guitarBadFinger)
    test "reject bad first fret offset" do
      validation (Assert.equal $ singleton "First fret offset should be between 0 and 27.")
        (const $ failure "bad fret offset expected")
        (FIVAL.validate Guitar.config guitarBadFretOffset)
    test "reject bad string number in barre" do
      validation (Assert.equal $ singleton "Invalid string number of 7 in the barré.")
        (const $ failure "bad barre expected")
        (FIVAL.validate Guitar.config guitarBadBarre)
    test "reject fingering hidden by barre" do
      validation (Assert.equal $ singleton "Fingering for string 3 is hidden by the barré.")
        (const $ failure "hidden by barre expected")
        (FIVAL.validate Guitar.config guitarHiddenByBarre)

tenorGuitarSuite :: Free TestF Unit
tenorGuitarSuite =
  suite "tenor guitar serialization" do
    test "write C chord" do
      Assert.equal tenorguitarCJSON $ writeFrettedInstrument tenorguitarC
    test "write F chord" do
      Assert.equal tenorguitarFJSON $ writeFrettedInstrument tenorguitarF
    test "read C chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal tenorguitarC)
        (FIVAL.validateJson TenorGuitar.config tenorguitarCJSON)
    test "read F chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal tenorguitarF)
        (FIVAL.validateJson TenorGuitar.config tenorguitarFJSON)
    test "read bad JSON" do
      validation (Assert.equal $ singleton "Not a recognisable tenor guitar chord format.")
        (const $ failure "bad JSON expected")
        (FIVAL.validateJson TenorGuitar.config badJSON)
    test "reject bad finger position" do
      validation (Assert.equal $ singleton "Finger position 50 is out of range.")
        (const $ failure "bad fingering expected")
        (FIVAL.validate TenorGuitar.config tenorGuitarBadFinger)
    test "reject bad first fret offset" do
      validation (Assert.equal $ singleton "First fret offset should be between 0 and 20.")
        (const $ failure "bad fret offset expected")
        (FIVAL.validate TenorGuitar.config tenorGuitarBadFretOffset)
    test "reject bad string number in barre" do
      validation (Assert.equal $ singleton "Invalid string number of 7 in the barré.")
        (const $ failure "bad barre expected")
        (FIVAL.validate TenorGuitar.config tenorGuitarBadBarre)
    test "reject fingering hidden by barre" do
      validation (Assert.equal $ singleton "Fingering for string 3 is hidden by the barré.")
        (const $ failure "hidden by barre expected")
        (FIVAL.validate TenorGuitar.config tenorGuitarHiddenByBarre)

bassSuite :: Free TestF Unit
bassSuite =
  suite "bass guitar serialization" do
    test "write G chord" do
      Assert.equal bassGJSON $ writeBass bassG
    test "read G chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal bassG)
        (BVAL.validateJson bassGJSON)
    test "read bad JSON" do
      validation (Assert.equal $ singleton "Not a recognisable bass chord format.")
        (const $ failure "bad JSON expected")
        (BVAL.validateJson badJSON)
    test "reject bad finger position" do
      validation (Assert.equal $ singleton "Finger position -2 is out of range.")
        (const $ failure "bad fingering expected")
        (BVAL.validate bassBadFinger)
    test "reject bad string count" do
      validation (Assert.equal $ singleton "Fingering for all 4 strings is required.")
        (const $ failure "bad string count expected")
        (BVAL.validate bassBadStringCount)
    test "reject bad first fret offset" do
      validation (Assert.equal $ singleton "First fret offset should be between 0 and 36.")
        (const $ failure "bad fret offset expected")
        (BVAL.validate bassBadFretOffset)

pianoF :: Piano.ChordShape
pianoF =
  { name : "F"
  , fingering : [5,9,12]
  }

pianoFJSON :: String
pianoFJSON =
  """{"name":"F","fingering":[5,9,12]}"""

pianoBadFinger :: Piano.ChordShape
pianoBadFinger =
  { name : "F"
  , fingering : [5,9,24]
  }

guitarA :: FrettedInstrument.ChordShape
guitarA =
  { name : "A"
  , firstFretOffset: 0
  , barre : Nothing
  , fingering :   [FrettedInstrument.open,FrettedInstrument.open,2,2,2,FrettedInstrument.open]
  }

guitarF :: FrettedInstrument.ChordShape
guitarF =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,3,2,2,FrettedInstrument.silent,FrettedInstrument.silent]
  }

guitarBadFinger :: FrettedInstrument.ChordShape
guitarBadFinger =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,50,2,2,FrettedInstrument.silent,FrettedInstrument.silent]
  }

guitarBadFretOffset :: FrettedInstrument.ChordShape
guitarBadFretOffset =
  { name : "F"
  , firstFretOffset: 50
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,3,2,2,FrettedInstrument.silent,FrettedInstrument.silent]
  }

guitarBadBarre :: FrettedInstrument.ChordShape
guitarBadBarre =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 7, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,3,2,2,FrettedInstrument.silent,FrettedInstrument.silent]
  }

guitarHiddenByBarre :: FrettedInstrument.ChordShape
guitarHiddenByBarre =
  { name : "G"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 3 }
  , fingering : [FrettedInstrument.silent,5,4,2,FrettedInstrument.silent,FrettedInstrument.silent]
  }

guitarAJSON :: String
guitarAJSON =
  """{"name":"A","firstFretOffset":0,"fingering":[0,0,2,2,2,0]}"""

guitarFJSON :: String
guitarFJSON =
  """{"name":"F","firstFretOffset":0,"fingering":[-1,3,2,2,-1,-1],"barre":{"stringNumber":0,"fretNumber":1}}"""

tenorguitarC :: FrettedInstrument.ChordShape
tenorguitarC =
  { name : "C"
  , firstFretOffset: 0
  , barre : Nothing
  , fingering :   [FrettedInstrument.open,FrettedInstrument.open,2,3]
  }

tenorguitarF :: FrettedInstrument.ChordShape
tenorguitarF =
  { name : "F"
  , firstFretOffset: 0
  , barre : Nothing
  , fingering : [FrettedInstrument.open,2,3,FrettedInstrument.open]
  }

tenorguitarCJSON :: String
tenorguitarCJSON =
  """{"name":"C","firstFretOffset":0,"fingering":[0,0,2,3]}"""

tenorguitarFJSON :: String
tenorguitarFJSON =
  """{"name":"F","firstFretOffset":0,"fingering":[0,2,3,0]}"""

tenorGuitarBadFinger :: FrettedInstrument.ChordShape
tenorGuitarBadFinger =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,50,2,FrettedInstrument.silent]
  }

tenorGuitarBadFretOffset :: FrettedInstrument.ChordShape
tenorGuitarBadFretOffset =
  { name : "F"
  , firstFretOffset: 50
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,3,2,FrettedInstrument.silent]
  }

tenorGuitarBadBarre :: FrettedInstrument.ChordShape
tenorGuitarBadBarre =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 7, fretNumber : 1 }
  , fingering : [FrettedInstrument.silent,3,2,FrettedInstrument.silent]
  }

tenorGuitarHiddenByBarre :: FrettedInstrument.ChordShape
tenorGuitarHiddenByBarre =
  { name : "G"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 3 }
  , fingering : [FrettedInstrument.silent,5,4,2]
  }

bassG :: Bass.ChordShape
bassG =
  { name : "G"
  , firstFretOffset: 0
  , fingering :   [ [{ fret : 3, status : Primary }]
                  , [{ fret : 2, status : Secondary }]
                  , [Bass.open]
                  , [Bass.open] ]
  }

bassGJSON :: String
bassGJSON =
  """{"name":"G","firstFretOffset":0,"fingering":["""  <>
  """[{"status":"Primary","fret":3}],""" <>
  """[{"status":"Secondary","fret":2}],""" <>
  """[{"status":"Primary","fret":0}],""" <>
  """[{"status":"Primary","fret":0}]]}"""

bassBadFinger :: Bass.ChordShape
bassBadFinger =
  { name : "G"
  , firstFretOffset: 0
  , fingering :   [ [{ fret : 3, status : Primary }]
                  , [{ fret : 2, status : Secondary }]
                  , [{ fret : -2, status : Secondary }]
                  , [Bass.open] ]
  }

bassBadStringCount :: Bass.ChordShape
bassBadStringCount =
  { name : "G"
  , firstFretOffset: 0
  , fingering :   [ [{ fret : 3, status : Primary }]
                  , [{ fret : 2, status : Secondary }]
                  , [Bass.open] ]
  }

bassBadFretOffset :: Bass.ChordShape
bassBadFretOffset =
  { name : "G"
  , firstFretOffset: -4
  , fingering :   [ [{ fret : 3, status : Primary }]
                  , [{ fret : 2, status : Secondary }]
                  , [Bass.open]
                  , [Bass.open] ]
  }


badJSON :: String
badJSON =
  """{"parameters": "ThisIsBad"}"""

badJSON1 :: String
badJSON1 =
  """{"name": "foo","parameters": "ThisIsBad"}"""
