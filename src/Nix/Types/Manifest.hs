{-# LANGUAGE UnicodeSyntax #-}
{-| work with nix `manifest.json` files -}

module Nix.Types.Manifest
  ( Manifest
  , attrPath
  , elements
  , getNameVerPathPrio
  , location
  , readManifestFile
  , tests
  , version
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson.KeyMap qualified as KeyMap

import Data.Aeson        ( FromJSON(parseJSON), Key, Result(Success),
                           Value(Array, Object), eitherDecodeStrict', fromJSON,
                           (.:) )
import Data.Aeson.KeyMap ( toMap, toMapText )
import Data.Aeson.Types  ( Parser, parseJSON, toJSON, withObject, withText )

-- base --------------------------------

import Data.Maybe   ( catMaybes, fromMaybe )
import GHC.Generics ( Generic )

-- containers --------------------------

import Data.Map.Lazy qualified as Map

-- fpath -------------------------------

import FPath.AbsDir  ( AbsDir )
import FPath.AbsFile ( AbsFile )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO ( DoMock )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( HasDoMock )

-- mockio-plus -------------------------

import MockIO.OpenFile ( readFile )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, ask )

-- text --------------------------------

import Data.Text ( pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.T.Helpers             ( checkFromJSON )
import Nix.Types                 ( Pkg(unPkg), Priority, Ver )
import Nix.Types.AttrPath        ( apPkg, pkg )
import Nix.Types.ManifestElement ( ManifestElement, attrPath, priority,
                                   storePaths )
import Nix.Types.StorePath       ( spPkgVerPath )

import Nix.Types.ManifestElement     qualified as ManifestElement
import Nix.Types.T.TestData.Manifest qualified as TestData

--------------------------------------------------------------------------------

newtype ManifestElementMap = ManifestElementMap { unManifestElementMap :: (Map.Map 𝕋 ManifestElement) }
  deriving (Eq, Generic, Show)

instance FromJSON ManifestElementMap

------------------------------------------------------------

-- newtype ManifestContents = ManifestContents (ℤ, [ManifestElement])
data ManifestContents = ManifestContents { version    :: ℤ
                                         , elementMap :: ManifestElementMap
                                         }
  deriving (Eq, Show)

--------------------

-- manifestContents ∷ ℤ → [ManifestElement] → ManifestContents
-- manifestContents v es = ManifestContents (v,es)
manifestContents ∷ ℤ → ManifestElementMap → ManifestContents
manifestContents v es = ManifestContents { version = v, elementMap = es }

----------------------------------------

rr ∷ Result α → α
rr (Success a) = a

readElements ∷ Value → Parser ManifestElementMap
readElements (Object o) =
  let parseKV (k,v) = do
        ḳ ← withText "elementKey" return (toJSON k)
        ṿ ← withObject "ManifestElement" (parseJSON ∘ toJSON) v
        return (ḳ,ṿ)
  in  ManifestElementMap ∘ Map.fromList ⩺ sequence $ parseKV ⊳ KeyMap.toList o

readElements (Array as) =
  let f e = (,e) ∘ unPkg ∘ view pkg ⊳ e ⊣ attrPath
  in    ManifestElementMap ∘ Map.fromList ⩺ fmap catMaybes ∘ sequence
      ∘ fmap (fmap f) ∘ fmap parseJSON $ toList as

instance FromJSON ManifestContents where
  parseJSON = withObject "ManifestContents" $ \ v →
    manifestContents ⊳ v .: "version" ⊵ (v .: "elements" ≫ readElements)

------------------------------------------------------------

{-| A profile manifest, read from a `manifest.json` file -}
data Manifest = Manifest { location :: AbsFile
                         , contents :: ManifestContents
                         }
  deriving (Eq, Generic, Show)

----------------------------------------

elements ∷ Manifest → [ManifestElement]
elements = Map.elems ∘ unManifestElementMap ∘ elementMap ∘ contents

----------------------------------------

mkManifest ∷ AbsFile → ManifestContents → Manifest
mkManifest f cs =  Manifest { location = f, contents = cs }

----------------------------------------

readManifestFile ∷ ∀ ε ω μ .
                   (MonadIO μ, MonadReader DoMock μ,
                    AsIOError ε, Printable ε, MonadError ε μ,
                    Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                   Severity → AbsFile → μ (𝔼 𝕊 Manifest)
readManifestFile sev f = do
  bs ← ask ≫ readFile sev 𝕹 (return "") f
  return $ mkManifest f ⊳ eitherDecodeStrict' bs

--------------------

instance Printable Manifest where
  print m =
    let getName e = case getNameVerPathPrio @TextualParseError e of
                      𝕷 err           → toText err
                      𝕽 𝕹             → pack $ show e
                      𝕽 (𝕵 (p,_,_,_)) → toText p

    in  P.text $ [fmt|manifest: %L|] [ getName e | e ← elements m ]

----------------------------------------

{-| extract the name, version & path from @ManifestElement@ -}
getNameVerPathPrio ∷ ∀ ε η . (AsTextualParseError ε, MonadError ε η) ⇒
                     ManifestElement → η (𝕄 (Pkg, 𝕄 Ver, AbsDir, 𝕄 Priority))
getNameVerPathPrio e =
  case head $ e ⊣ storePaths of
    𝕹 → return 𝕹
    𝕵 p → 𝕵 ⊳ do
      (pkgs,ver,path) ← spPkgVerPath ⊳ tparse p
      let prio = e ⊣ priority
      case e ⊣ attrPath of
        𝕵 ap → (,ver,path,prio) ⊳ apPkg (toText ap)
        𝕹    → return (pkgs,ver,path,prio)


-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Manifest"
    [ testGroup "fromJSON"
      [ checkFromJSON "v2Manifest" TestData.v2Manifest
          (let elems = [ ("chrysalis", ManifestElement.manifestElement1)
                       , ("gqview"   , ManifestElement.manifestElement2) ]
           in  ManifestContents { version = 2
                                , elementMap = ManifestElementMap $ Map.fromList elems })
      , checkFromJSON "v3Manifest" TestData.v3Manifest
          (let elems = [ ("chrysalis", ManifestElement.manifestElement1)
                       , ("gqview"   , ManifestElement.manifestElement2) ]
           in  ManifestContents { version = 3, elementMap = ManifestElementMap $ Map.fromList elems })
      ]
    ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
