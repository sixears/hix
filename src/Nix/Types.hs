{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnicodeSyntax      #-}

{-| miscellaneous small types used in nix profile representation -}
module Nix.Types
  ( Arch(unArch)
  , Hash(unHash)
  , Pkg(Pkg, unPkg)
  , PkgMVer(unPkgMVer)
  , Priority(Priority, unPriority)
  , ProfileDir(ProfileDir, unProfileDir)
  , RemoteState(..)
  , Ver(unVer)
  , remoteArgs
  , x86_64Linux
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson ( FromJSON, FromJSONKey )

-- base --------------------------------

import Data.Char ( isAlpha, isAlphaNum )
import Data.List ( intercalate )
import GHC.Exts  ( IsString(fromString) )
import Text.Read ( read )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.AbsDir ( AbsDir )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, satisfy )
import Text.Parser.Combinators ( optional, try )

-- text --------------------------------

import Data.Text ( pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types.ConfigDir   ( ConfigDir, configNameFromDir )
import Nix.Types.ToBriefText ( ToBriefText(toT) )

--------------------------------------------------------------------------------

nixosCache ∷ 𝕋
nixosCache = "https://cache.nixos.org/"

------------------------------------------------------------

data RemoteState = FullyConnected | Remote | Isolated deriving (Show)

------------------------------------------------------------

{-| a nix (linux) architecture -}
newtype Arch = Arch { unArch :: 𝕋 }
  deriving newtype (Eq, FromJSONKey, IsString, Ord, Printable, Show)

{-| @Arch@ label for x86_64-linux -}
x86_64Linux ∷ Arch
x86_64Linux = "x86_64-linux"

------------------------------------------------------------

{-| a nix package hash -}
newtype Hash = Hash { unHash :: 𝕋 }
  deriving newtype (Eq, IsString, Printable, Show)

------------------------------------------------------------

{-| a nix package name -}
newtype Pkg = Pkg { unPkg :: 𝕋 }
  deriving newtype (Eq, FromJSONKey, IsString, NFData, Ord, Printable, Show)

instance TextualPlus Pkg where
  textual' =
    let alpha_under_score      ∷ CharParsing η ⇒ η ℂ
        alpha_under_score      = satisfy (\ c → isAlpha c ∨ c ≡ '_')
        non_hyphen             ∷ CharParsing η ⇒ η ℂ
        non_hyphen             = satisfy (\ c → isAlphaNum c ∨ c ∈ "_.")
        simple_identifier      ∷ CharParsing η ⇒ η 𝕊
        simple_identifier      = (:) ⊳ alpha_under_score ⊵ many non_hyphen
    in  (Pkg ∘ pack) ⊳ intercalate "-" ⊳ ((:) ⊳ simple_identifier
                                       ⊵ many(try $char '-' ⋫simple_identifier))


------------------------------------------------------------

{-| a nix profile manifest priority -}
newtype Priority = Priority { unPriority :: ℕ }
  deriving newtype (Eq, FromJSON, NFData, Ord, Show)

instance Printable Priority where
  print (Priority n) = P.text $ [fmt|%d|] n

instance TextualPlus Priority where
  textual' = Priority ∘ read ⊳ some digit

------------------------------------------------------------

{-| a nix package version -}
newtype Ver = Ver { unVer :: 𝕋 }
  deriving newtype (Eq, IsString, Printable, Show)

instance TextualPlus Ver where
  textual' = let alNumHypUnderDot = satisfy (\c → isAlphaNum c ∨ c ∈ "-_.")
             in  fromString ⊳ ((:) ⊳ digit ⊵ many alNumHypUnderDot)

------------------------------------------------------------

{-| a nix package version -}
newtype ProfileDir = ProfileDir { unProfileDir :: AbsDir }
  deriving newtype (Eq, Printable, Show)

instance ToBriefText ProfileDir where
  toT = toT ∘ configNameFromDir ∘ unProfileDir

------------------------------------------------------------

instance Printable (ConfigDir,ProfileDir) where
  print (c,p) = P.text $ [fmt|%T→%T|] c p

------------------------------------------------------------

newtype PkgMVer = PkgMVer { unPkgMVer :: (Pkg, 𝕄 Ver) }

instance TextualPlus PkgMVer where
  textual' = PkgMVer ⊳ ((,) ⊳ textual' ⊵ optional (char '-' ⋫ textual'))

----------------------------------------

nixOption ∷ (𝕋,𝕋) → [𝕋]
nixOption (k,v) = [ "--option", k, v ]

substituters ∷ 𝕄 𝕋 → [𝕋]
substituters 𝕹     = []
substituters (𝕵 x) = nixOption ("substituters",x)

remoteArgs ∷ RemoteState → [𝕋]
remoteArgs r = substituters (go r)
               where go FullyConnected = 𝕹
                     go Isolated       = 𝕵 ""
                     go Remote         = 𝕵 nixosCache

-- that's all, folks! ----------------------------------------------------------
