{-# LANGUAGE UnicodeSyntax #-}
module Nix.Profile
  ( AsProfileDir(..)
  ) where

import Base1T

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, absdir )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Error.FPathError ( AsFPathError )
import FPath.Parseable        ( parse, parseDir )
import FPath.PathComponent    ( PathComponent, pc )
import FPath.RelDir           ( RelDir )
import FPath.RelFile          ( RelFile )
import FPath.ToDir            ( toDir )

-- monadio-plus ------------------------

import MonadIO.FPath ( pResolve )
import MonadIO.User  ( getUserName' )

-- non-empty-containers ----------------

-- import NonEmptyContainers.IsNonEmpty ( fromNonEmpty )

-- text --------------------------------

import Data.Text qualified

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types ( ProfileDir(ProfileDir) )

--------------------------------------------------------------------------------

{-| where to find per-user profiles on a nix-enabled host -}
perUserProfiles ∷ AbsDir
perUserProfiles = [absdir|/nix/var/nix/profiles/per-user/|]

{- | user name, as a `ProfileName` -}
userProfileName ∷ ∀ ε μ .
              (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
              μ ProfileName
userProfileName = ProfileName ⊳ (getUserName' ≫ parse)

userProfileNameRelDir ∷ ∀ ε μ .
                        (MonadIO μ,
                         AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
                        μ RelDir
userProfileNameRelDir = (fromList ∘ pure ∘ unProfileName) ⊳ userProfileName

{-| top dir where nix profiles are stored -}
profilesTop ∷ (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
              μ AbsDir
profilesTop = (perUserProfiles ⫻) ⊳ userProfileNameRelDir

----------------------------------------

{-| append a profile name to a dir to find a profile dir -}
profileAppend ∷ AbsDir → ProfileName → AbsDir
profileAppend top = (top ⫻) ∘ fromList ∘ pure ∘ unProfileName

----------------------------------------

{-| find a profile dir from a profile name, assuming the use of `profilesTop` -}
profileDir ∷ (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
             ProfileName → μ ProfileDir
profileDir = ProfileDir ⩺ (profilesTop ⊲) ∘ flip profileAppend

----------------------------------------

newtype ProfileName = ProfileName { unProfileName :: PathComponent }
  deriving (Printable)

------------------------------------------------------------

class AsProfileDir α where
  {-| The one true dir for a profile, found under `/nix`. -}
  nixProfileAbsDir ∷ α → (MonadIO μ, AsFPathError ε, AsIOError ε,
                          Printable ε, MonadError ε μ) ⇒
                     μ ProfileDir

--------------------

instance AsProfileDir ProfileName where
  nixProfileAbsDir p = profileDir p

--------------------

defaultAbsProfile ∷ ∀ ε μ . (MonadIO μ, MonadError ε μ,
                             AsIOError ε, AsFPathError ε, Printable ε) ⇒
                    μ ProfileDir
defaultAbsProfile = profileDir (ProfileName [pc|profile|])

instance AsProfileDir 𝕋 where
  nixProfileAbsDir "" = defaultAbsProfile
  nixProfileAbsDir p = do
    userName ← getUserName' ≫ parseDir ∘ toText
    case (≡ '/') `Data.Text.find` p of
      𝓝   → do n ← parse @RelFile p
               return ∘ ProfileDir $ perUserProfiles ⫻ userName ⫻ toDir n
      𝓙 _ → ProfileDir ⊳ pResolve p

instance AsProfileDir (𝕄 𝕋) where
  nixProfileAbsDir 𝓝     = defaultAbsProfile
  nixProfileAbsDir (𝓙 t) = nixProfileAbsDir t

-- that's all, folks! ----------------------------------------------------------
