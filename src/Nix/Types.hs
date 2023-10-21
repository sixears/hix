{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnicodeSyntax      #-}

{-| miscellaneous small types used in nix profile representation -}
module Nix.Types
  ( Arch(unArch)
  , ConfigDir(ConfigDir, unConfigDir)
  , ConfigName(ConfigName, unConfigName)
  , Hash(unHash)
  , Pkg(Pkg, unPkg)
  , ProfileDir(ProfileDir, unProfileDir)
  , RemoteState(..)
  , ToBriefText(toT)
  , Ver(unVer)
  , configDirName
  , configNameFromDir
  , pkgRE
  , remoteArgs
  , x86_64Linux
  ) where

import Base1T

import Prelude ( error )

-- aeson -------------------------------

import Data.Aeson ( FromJSONKey )

-- base --------------------------------

import Data.Char ( isAlpha, isAlphaNum )
import Data.List ( intercalate )
import Data.Ord  ( Ord(compare) )
import GHC.Exts  ( IsString(fromString) )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.AbsDir        ( AbsDir )
import FPath.Basename      ( Basename, basename )
import FPath.Dir           ( DirAs )
import FPath.Parseable     ( __parse'__ )
import FPath.PathComponent ( PathComponent )
import FPath.RelType       ( RelType )

-- mono-traversable --------------------

import Data.MonoTraversable ( Element, MonoFoldable, otoList )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, lower, satisfy )
import Text.Parser.Combinators ( choice, optional, try )

-- safe --------------------------------

import Safe ( lastMay )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

nixosCache ∷ 𝕋
nixosCache = "https://cache.nixos.org/"

------------------------------------------------------------

class ToBriefText α where
  toT ∷ α → 𝕋

instance ToBriefText 𝕋 where
  toT = id

instance (ToBriefText α, ToBriefText β) ⇒ ToBriefText (α,β) where
  toT (a,b) =
    let a' = toT a
        b' = toT b
    in  if a' ≡ b' then a' else [fmt|%t→%t|] (toT a) (toT b)

------------------------------------------------------------

newtype ConfigName = ConfigName { unConfigName :: PathComponent }
  deriving (Eq, Printable, Show)

instance Ord ConfigName where
  compare (ConfigName p) (ConfigName p') = compare (toText p) (toText p')

instance TextualPlus ConfigName where
  textual' = let parse_text = (:) ⊳ lower ⊵ many (choice [lower,digit,char '-'])
             in  ConfigName ∘ __parse'__ ⊳ parse_text

instance ToBriefText ConfigName where
  toT (ConfigName c) = toText c

------------------------------------------------------------

newtype ConfigDir = ConfigDir { unConfigDir :: AbsDir }
  deriving (Printable, Show)

instance ToBriefText ConfigDir where
  toT = toT ∘ configDirName

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

------------------------------------------------------------

{-| a nix package version -}
newtype Ver = Ver { unVer :: 𝕋 }
  deriving newtype (Eq, IsString, Printable, Show)

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

pkgRE ∷ CharParsing η ⇒ η (Pkg, 𝕄 Ver)
pkgRE =
  let
    alpha_under_score      ∷ CharParsing η ⇒ η ℂ
    alpha_under_score      = satisfy (\ c → isAlpha c ∨ c ≡ '_')
    non_hyphen             ∷ CharParsing η ⇒ η ℂ
    non_hyphen             = satisfy (\ c → isAlphaNum c ∨ c ∈ "_.")
    simple_identifier      ∷ CharParsing η ⇒ η 𝕊
    simple_identifier      = (:) ⊳ alpha_under_score ⊵ many non_hyphen
    hyphenated_identifiers ∷ CharParsing η ⇒ η 𝕊
    hyphenated_identifiers =
      intercalate "-" ⊳ ((:) ⊳ simple_identifier ⊵many(try $ char '-' ⋫simple_identifier))
    numeric_identifier     ∷ CharParsing η ⇒ η 𝕊
    numeric_identifier     =
      (:) ⊳ digit ⊵ many (satisfy (\ c → isAlphaNum c ∨ c ∈ "-_."))
    fromStr p v = (fromString p, fromString ⊳ v)
  in
    (fromStr ⊳ hyphenated_identifiers ⊵ optional(char '-' ⋫ numeric_identifier))

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

configDirName ∷ ConfigDir → ConfigName
configDirName = configNameFromDir ∘ unConfigDir

configNameFromDir ∷ (DirAs δ, Element (RelType δ) ~ PathComponent,
                     MonoFoldable (RelType δ), Basename δ) ⇒ δ → ConfigName
configNameFromDir d = case lastMay ∘ otoList $ basename d of
                        𝕹   → error $ [fmt|could not find ConfigName of %T|] d
                        𝕵 p → ConfigName p

-- that's all, folks! ----------------------------------------------------------
