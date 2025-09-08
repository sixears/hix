{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.ManifestElement
  ( ManifestElement(ManifestElement)
  , active
  , attrPath
  , manifestElement1
  , manifestElement2
  , originalURL
  , priority
  , storePaths
  , tests
  , url
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson.Types qualified as AesonTypes

import Data.Aeson ( FromJSON(parseJSON), defaultOptions, fieldLabelModifier,
                    genericParseJSON )

-- base --------------------------------

import GHC.Generics ( Generic )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.T.Helpers      ( checkFromJSON )
import Nix.Types          ( Priority(Priority) )
import Nix.Types.AttrPath ( AttrPath )

import Nix.Types.T.TestData.ManifestElement qualified as TestData

--------------------------------------------------------------------------------

{-| An individual element of a profile manifest -}
data ManifestElement = ManifestElement { _active      :: 𝔹
                                       , _priority    :: 𝕄 Priority
                                       , _storePaths  :: [𝕋]
                                       , _attrPath    :: 𝕄 AttrPath
                                       , _originalURL :: 𝕄 𝕋
                                       , _url         :: 𝕄 𝕋
                                       }
  deriving (Eq, Generic, Show)

instance FromJSON ManifestElement where
  parseJSON =
    let customOptions ∷ AesonTypes.Options
        customOptions =
          let dropLeadingUnderscore ('_' : s) = s
              dropLeadingUnderscore s         = s
          in  defaultOptions { fieldLabelModifier = dropLeadingUnderscore }
    in  genericParseJSON customOptions

active ∷ Lens' ManifestElement 𝔹
active = lens _active (\ me a → me { _active = a })

priority ∷ Lens' ManifestElement (𝕄 Priority)
priority = lens _priority (\ me a → me { _priority = a })

storePaths ∷ Lens' ManifestElement [𝕋]
storePaths = lens _storePaths (\ me a → me { _storePaths = a })

attrPath ∷ Lens' ManifestElement (𝕄 AttrPath)
attrPath = lens _attrPath (\ me a → me { _attrPath = a })

originalURL ∷ Lens' ManifestElement (𝕄 𝕋)
originalURL = lens _originalURL (\ me a → me { _originalURL = a })

url ∷ Lens' ManifestElement (𝕄 𝕋)
url = lens _url (\ me a → me { _url = a })

-- tests -----------------------------------------------------------------------

manifestElement1 ∷ ManifestElement
manifestElement1 =
  ManifestElement { _active = 𝓣
                  , _priority = 𝓙 (Priority 5)
                  , _storePaths = [TestData.storePath1]
                  , _attrPath = 𝓙 TestData.attrPath1
                  , _originalURL = 𝓝
                  , _url = 𝓙 $ toText TestData.url1
                  }

--------------------

manifestElement2 ∷ ManifestElement
manifestElement2 =
  ManifestElement { _active = 𝓣
                  , _priority = 𝓙 (Priority 3)
                  , _storePaths = [TestData.storePath2]
                  , _attrPath = 𝓙 TestData.attrPath2
                  , _originalURL = 𝓝
                  , _url = 𝓙 $ toText TestData.url2
                  }

--------------------
{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "ManifestElement"
    [ testGroup "fromJSON"
      [ checkFromJSON "element1" TestData.manifestElement1BS manifestElement1
      , checkFromJSON "element2" TestData.manifestElement2BS manifestElement2
      ]
    ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
