{-# LANGUAGE UnicodeSyntax #-}
module Nix.Profile
  ( AsProfileDir(..)
  ) where

import Base1T

-- base --------------------------------

import Data.Function ( flip )

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, absdir )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Dir              ( Dir(DirA, DirR) )
import FPath.Error.FPathError ( AsFPathError )
import FPath.Parseable        ( parse, parseDir )
import FPath.PathComponent    ( PathComponent, pc )
import FPath.RelDir           ( RelDir, reldir )
import FPath.RelFile          ( RelFile )
import FPath.ToDir            ( toDir )

-- monadio-plus ------------------------

import MonadIO.FPath ( pResolve )
import MonadIO.User  ( getUserName', homePath )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty ( fromNonEmpty )

-- text --------------------------------

import Data.Text qualified

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
             ProfileName → μ AbsDir
profileDir = (profilesTop ⊲) ∘ flip profileAppend

----------------------------------------

{-| Where we expect to find local symlinks to nix profiles. -}
homeNixProfiles ∷ (AsIOError ε, AsFPathError ε, MonadError ε μ, MonadIO μ) ⇒
                  μ AbsDir
homeNixProfiles = homePath [reldir|.nix-profiles/|]

------------------------------------------------------------

newtype ProfileName = ProfileName { unProfileName :: PathComponent }
  deriving (Printable)

------------------------------------------------------------

class AsProfileDir α where
  {-| The one true dir for a profile, found under `/nix`. -}
  nixProfileAbsDir ∷ α → (MonadIO μ, AsFPathError ε, AsIOError ε,
                          Printable ε, MonadError ε μ) ⇒
                     μ AbsDir
  {-| The dir for a profile, found under `~/.nix-profiles` -}
  nixProfileLocalDir ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ)⇒
                       α → μ AbsDir

--------------------

instance AsProfileDir ProfileName where
  nixProfileAbsDir p = profileDir p

  nixProfileLocalDir p =
    homeNixProfiles ⊲ (⫻ fromNonEmpty (pure $ unProfileName p))

--------------------

defaultAbsProfile ∷ ∀ ε μ . (MonadIO μ, MonadError ε μ,
                             AsIOError ε, AsFPathError ε, Printable ε) ⇒
                    μ AbsDir
defaultAbsProfile = profileDir (ProfileName [pc|profile|])
defaultLocalProfile ∷ ∀ ε μ . (MonadIO μ, MonadError ε μ,
                               AsIOError ε, AsFPathError ε) ⇒
                      μ AbsDir
defaultLocalProfile = homePath [reldir|.nix-profile/|]

instance AsProfileDir (𝕄 𝕋) where
  nixProfileAbsDir 𝕹 = defaultAbsProfile
  nixProfileAbsDir (𝕵 "") = defaultAbsProfile
  nixProfileAbsDir (𝕵 p) = do
    userName ← getUserName' ≫ parseDir ∘ toText
    case (≡ '/') `Data.Text.find` p of
      𝕹   → do n ← parse @RelFile p
               return $ perUserProfiles ⫻ userName ⫻ toDir n
      𝕵 _ → pResolve p

  nixProfileLocalDir 𝕹 = defaultLocalProfile
  nixProfileLocalDir (𝕵 "") = defaultLocalProfile
  nixProfileLocalDir (𝕵 p) = parseDir p ≫ \ case
    DirR p' → homeNixProfiles ⊲ (⫻ p')
    DirA p' → return p'

-- that's all, folks! ----------------------------------------------------------
