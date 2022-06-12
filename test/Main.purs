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
import Guitar.Types as Guitar
import Guitar.Validation (validate, validateJson) as GVAL
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
      Assert.equal guitarAJSON $ writeGuitar guitarA
    test "write F chord" do
      Assert.equal guitarFJSON $ writeGuitar guitarF
    test "read A chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal guitarA)
        (GVAL.validateJson guitarAJSON)
    test "read F chord" do
      validation (const $ failure "successful validation expected")
        (Assert.equal guitarF)
        (GVAL.validateJson guitarFJSON)
    test "read bad JSON" do
      validation (Assert.equal $ singleton "Not a recognisable guitar chord format.")
        (const $ failure "bad JSON expected")
        (GVAL.validateJson badJSON)
    test "reject bad finger position" do
      validation (Assert.equal $ singleton "Finger position 50 is out of range.")
        (const $ failure "bad fingering expected")
        (GVAL.validate guitarBadFinger)
    test "reject bad first fret offset" do
      validation (Assert.equal $ singleton "First fret offset should be between 0 and 27.")
        (const $ failure "bad fret offset expected")
        (GVAL.validate guitarBadFretOffset)
    test "reject bad string number in barre" do
      validation (Assert.equal $ singleton "Invalid string number of 7 in the barré.")
        (const $ failure "bad barre expected")
        (GVAL.validate guitarBadBarre)
    test "reject fingering hidden by barre" do
      validation (Assert.equal $ singleton "Fingering for string 3 is hidden by the barré.")
        (const $ failure "hidden by barre expected")
        (GVAL.validate guitarHiddenByBarre)

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

guitarA :: Guitar.ChordShape
guitarA =
  { name : "A"
  , firstFretOffset: 0
  , barre : Nothing
  , fingering :   [Guitar.open,Guitar.open,2,2,2,Guitar.open]
  }

guitarF :: Guitar.ChordShape
guitarF =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [Guitar.silent,3,2,2,Guitar.silent,Guitar.silent]
  }

guitarBadFinger :: Guitar.ChordShape
guitarBadFinger =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [Guitar.silent,50,2,2,Guitar.silent,Guitar.silent]
  }

guitarBadFretOffset :: Guitar.ChordShape
guitarBadFretOffset =
  { name : "F"
  , firstFretOffset: 50
  , barre : Just { stringNumber : 0, fretNumber : 1 }
  , fingering : [Guitar.silent,3,2,2,Guitar.silent,Guitar.silent]
  }

guitarBadBarre :: Guitar.ChordShape
guitarBadBarre =
  { name : "F"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 7, fretNumber : 1 }
  , fingering : [Guitar.silent,3,2,2,Guitar.silent,Guitar.silent]
  }

guitarHiddenByBarre :: Guitar.ChordShape
guitarHiddenByBarre =
  { name : "G"
  , firstFretOffset: 0
  , barre : Just { stringNumber : 0, fretNumber : 3 }
  , fingering : [Guitar.silent,5,4,2,Guitar.silent,Guitar.silent]
  }

guitarAJSON :: String
guitarAJSON =
  """{"name":"A","firstFretOffset":0,"fingering":[0,0,2,2,2,0]}"""

guitarFJSON :: String
guitarFJSON =
  """{"name":"F","firstFretOffset":0,"fingering":[-1,3,2,2,-1,-1],"barre":{"stringNumber":0,"fretNumber":1}}"""

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
