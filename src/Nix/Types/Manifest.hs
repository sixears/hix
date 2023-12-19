{-# LANGUAGE UnicodeSyntax #-}
{-| work with nix `manifest.json` files -}

module Nix.Types.Manifest
  ( Manifest
  , ManifestElement
  , attrPath
  , elements
  , getNameVerPathPrio
  , location
  , readManifestFile
  , version
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson ( FromJSON(parseJSON), eitherDecodeStrict', withObject, (.:) )

-- base --------------------------------

import GHC.Generics ( Generic )

-- fpath -------------------------------

import FPath.AbsDir  ( AbsDir )
import FPath.AbsFile ( AbsFile )

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

import Data.Text  ( pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types           ( Pkg, Priority, Ver )
import Nix.Types.AttrPath  ( AttrPath, apPkg )
import Nix.Types.StorePath ( spPkgVerPath )

--------------------------------------------------------------------------------

{-| An individual element of a profile manifest -}
data ManifestElement = ManifestElement { active      :: 𝔹
                                       , priority    :: 𝕄 Priority
                                       , storePaths  :: [𝕋]
                                       , attrPath    :: 𝕄 AttrPath
                                       , originalURL :: 𝕄 𝕋
                                       , url         :: 𝕄 𝕋
                                       }
  deriving (Generic, Show)

instance FromJSON ManifestElement

------------------------------------------------------------

{-| A profile manifest, read from a `manifest.json` file -}
data Manifest = Manifest { version  :: ℤ
                         , location :: AbsFile
                         , elements :: [ManifestElement]
                         }
  deriving (Generic, Show)

newtype ManifestContents = ManifestContents (ℤ, [ManifestElement])

manifestContents ∷ ℤ → [ManifestElement] → ManifestContents
manifestContents v es = ManifestContents (v,es)

mkManifest ∷ AbsFile → ManifestContents → Manifest
mkManifest f (ManifestContents (v,es)) =
  Manifest { version = v, location = f, elements = es }

instance FromJSON ManifestContents where
  parseJSON = withObject "Manifest" $ \ v →
    manifestContents ⊳ v .: "version" ⊵ v .: "elements"

readManifestFile ∷ ∀ ε ω μ .
                   (MonadIO μ, MonadReader DoMock μ,
                    AsIOError ε, Printable ε, MonadError ε μ,
                    Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                   Severity → AbsFile → μ (𝔼 𝕊 Manifest)
readManifestFile sev f = do
  bs ← ask ≫ readFile sev 𝕹 (return "") f
  liftIO $ mkManifest f ⊳⊳ return (eitherDecodeStrict' bs)

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
  case head $ storePaths e of
    𝕹 → return 𝕹
    𝕵 p → 𝕵 ⊳ do
      (pkgs,ver,path) ← spPkgVerPath ⊳ tparse p
      let prio = priority e
      case attrPath e of
        𝕵 ap → (,ver,path,prio) ⊳ apPkg (toText ap)
        𝕹    → return (pkgs,ver,path,prio)

-- that's all, folks! ----------------------------------------------------------
