{-| work with nix `manifest.json` files -}

module Nix.Profile.Manifest
  ( Manifest, ManifestElement, elementsi, getNameVerPath, readManifest )
where

import Base1T

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, eitherDecodeFileStrict' )

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Data.List     ( zip )
import GHC.Generics  ( Generic )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AsFilePath        ( filepath )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Dir               ( Dir( DirA, DirR ) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parseable         ( parseDir )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational ) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( NoMock ), HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass  ( HasIOClass )

-- mockio-plus -------------------------

import MockIO.File  ( fexists )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( throwUserError )

-- monadio-plus ------------------------

import MonadIO.FStat  ( FExists( FExists, NoFExists ) )
import MonadIO.User   ( homePath )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (⋘) )

-- textual-plus ------------------------

import TextualPlus                         ( tparse )
import TextualPlus.Error.TextualParseError ( TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Profile.AttrPath   ( apPkg )
import Nix.Profile.StorePath  ( spPkgVerPath )
import Nix.Types              ( Pkg, Ver )

--------------------------------------------------------------------------------

{-| An individual element of a profile manifest -}
data ManifestElement = ManifestElement { active      ∷ 𝔹
                                       , priority    ∷ ℕ
                                       , storePaths  ∷ NonEmpty 𝕊
                                       , attrPath    ∷ 𝕄 𝕊
                                       , originalURL ∷ 𝕄 𝕊
                                       , url         ∷ 𝕄 𝕊
                                       }
  deriving (Generic, Show)

instance FromJSON ManifestElement

------------------------------------------------------------

{-| A profile manifest, read from a `manifest.json` file -}
data Manifest = Manifest { version ∷ ℤ, elements ∷ [ManifestElement] }
  deriving (Generic, Show)

instance FromJSON Manifest

------------------------------------------------------------

nixProfile ∷ (AsIOError ε,AsFPathError ε,MonadError ε μ,MonadIO μ) ⇒ μ AbsDir
nixProfile = homePath [reldir|.nix-profile/|]

----------------------------------------

nixProfiles ∷ (AsIOError ε,AsFPathError ε,MonadError ε μ,MonadIO μ) ⇒ μ AbsDir
nixProfiles = homePath [reldir|.nix-profiles/|]

------------------------------------------------------------

{-| Given a profile name, return the manifest filepath.  If the name is the
    empty string, return the default profile (~/.nix-profile) -}
profileManifest ∷ ∀ ε τ ω μ .
                  (AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                   HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ,
                   Printable τ, MonadIO μ) ⇒
                  τ → μ AbsFile
profileManifest (toText → d) = do
  dir ← if d ≡ ""
        then nixProfile
        else parseDir d ≫ \ case
             DirR d' → nixProfiles ⊲ (⫻ d')
             DirA d' → return d'

  fexists Informational FExists dir NoMock ≫ \ case
    NoFExists → throwUserError $ [fmtT|No such profile dir '%T'|] dir
    FExists   → let manifest_json = [relfile|manifest.json|]
                    manifest      = dir ⫻ manifest_json
                in  fexists Informational FExists manifest NoMock ≫ \ case
                      FExists   → return manifest
                      NoFExists → throwUserError $
                                    [fmtT|profile dir '%T' lacks a %T|]
                                    dir manifest_json

----------------------------------------

{-| Given a name (e.g., "default", or "desktop"; or the empty string meaning the
    default profile from ~/.nix-profile/; read the `manifest.json` file for that
    profile.
 -}
readManifest ∷ ∀ ε τ ω μ .
               (AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ,
                Printable τ, MonadIO μ) ⇒
               τ → μ (Either 𝕊 Manifest)
readManifest =
  liftIO ∘ eitherDecodeFileStrict' ∘ (⫥ filepath) ⋘ profileManifest

----------------------------------------

{-| elements in a manifest, along with a zero-based index -}
elementsi ∷ Manifest → [(ℕ,ManifestElement)]
elementsi m = zip [0..] (elements m)

----------------------------------------

{-| extract the name, version & path from @ManifestElement@ -}
getNameVerPath ∷ (MonadError TextualParseError η) ⇒
                 ManifestElement → η (Pkg, 𝕄 Ver, AbsDir)
getNameVerPath e = do
  (pkgs,ver,path) ← spPkgVerPath ⊳ tparse (NonEmpty.head $ storePaths e)
  case attrPath e of
    𝕵 ap → (,ver,path) ⊳ apPkg ap
    𝕹    → return (pkgs,ver,path)

-- that's all, folks! ----------------------------------------------------------
