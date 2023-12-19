{-# LANGUAGE UnicodeSyntax #-}
{-| work with nix `manifest.json` files -}

module Nix.Profile.Manifest
  ( attrPaths
  , elementsi
  , findPkg
  , findPkg'
  , findPkgs
  , readManifest
  , readManifestDir
  ) where

import Base1T

-- base --------------------------------

import Data.List  ( elemIndices, zip )
import Data.Maybe ( catMaybes )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Error.FPathError ( AsFPathError )
import FPath.RelFile          ( relfile )

-- lens --------------------------------

import Control.Lens.Getter ( view )
import Control.Lens.Tuple  ( _1 )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock ( DoMock(NoMock), HasDoMock )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )

-- mockio-plus -------------------------

import MockIO.File ( fexists )

-- monaderror-io -----------------------

import MonadError.IO.Error ( throwUserError )

-- monadio-plus ------------------------

import MonadIO.FStat ( FExists(FExists, NoFExists) )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad ( (⋘) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Error          ( AsNixDuplicatePkgError, AsNixError,
                            throwAsNixDuplicatePkgError,
                            throwAsNixErrorDuplicatePkg )
import Nix.Profile        ( nixProfileAbsDir )
import Nix.Types          ( Pkg, ProfileDir(unProfileDir) )
import Nix.Types.AttrPath ( AttrPath )
import Nix.Types.Manifest ( Manifest, ManifestElement, attrPath, elements,
                            getNameVerPathPrio, location, readManifestFile )

--------------------------------------------------------------------------------

{-| Given a profile name, return the manifest filepath.  If the name is the
    empty string, return the default profile (~/.nix-profile) -}
profileManifest ∷ ∀ ε τ ω μ .
                  (AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                   HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ,
                   Printable τ, MonadIO μ) ⇒
                  (𝕄 τ) → μ AbsFile
profileManifest (fmap toText → d) = do
  dir ← unProfileDir ⊳ nixProfileAbsDir d

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

{-| Given a dir; read the @manifest.json@ profile file from that dir. -}
readManifestDir ∷ ∀ ε ω μ .
               (MonadIO μ, MonadReader DoMock μ,
                AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ) ⇒
               Severity → ProfileDir → μ (𝔼 𝕊 Manifest)
readManifestDir sev =
  readManifestFile sev ∘ (⫻ [relfile|manifest.json|]) ∘ unProfileDir

{-| Given a name (e.g., "default", or "desktop"; or the empty string meaning the
    default profile from ~/.nix-profile/; read the `manifest.json` file for that
    profile.
 -}
readManifest ∷ ∀ ε τ ω μ .
               (MonadIO μ, MonadReader DoMock μ,
                AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                HasIOClass ω, HasDoMock ω, Default ω, MonadLog (Log ω) μ,
                Printable τ) ⇒
               Severity → 𝕄 τ → μ (𝔼 𝕊 Manifest)
readManifest sev = readManifestFile sev ⋘ profileManifest

----------------------------------------

{-| elements in a manifest, along with a zero-based index -}
elementsi ∷ Manifest → [(ℕ,ManifestElement)]
elementsi m = zip [0..] (elements m)

----------------------------------------

findPkgs ∷ ∀ ε η . (AsTextualParseError ε, MonadError ε η)⇒
           Pkg → Manifest → η [ℕ]
findPkgs p m = do
  pkgs ← view _1 ⊳⊳
              (catMaybes ⊳ sequence [ (getNameVerPathPrio e) | e ← elements m ])
  return $ fromIntegral ⊳ elemIndices p pkgs

findPkg_ ∷ ∀ ε η . (AsTextualParseError ε, MonadError ε η)⇒
           (Pkg → AbsFile → η (𝕄 ℕ)) → Pkg → Manifest → η (𝕄 ℕ)
findPkg_ throw p m = do
  findPkgs p m ≫ \ case
    []   → return 𝕹
    [p'] → return ∘ 𝕵 $ p'
    _    → throw p (location m)

findPkg ∷ ∀ ε η.(AsNixDuplicatePkgError ε,AsTextualParseError ε,MonadError ε η)⇒
          Pkg → Manifest → η (𝕄 ℕ)
findPkg = findPkg_ throwAsNixDuplicatePkgError

findPkg' ∷ (AsNixError ε, AsTextualParseError ε, MonadError ε η) ⇒
           Pkg → Manifest → η (𝕄 ℕ)
findPkg' = findPkg_ throwAsNixErrorDuplicatePkg

----------------------------------------

attrPaths ∷ Manifest → [AttrPath]
attrPaths m = catMaybes [ attrPath e | e ← elements m ]

-- that's all, folks! ----------------------------------------------------------
