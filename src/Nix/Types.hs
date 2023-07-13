{-# LANGUAGE DerivingStrategies #-}

{-| miscellaneous small types used in nix profile representation -}
module Nix.Types
  ( Arch( unArch ), Hash( unHash ), Pkg( unPkg ), Ver( unVer )
  , pkgRE, x86_64Linux )
where

import Base1T

-- aeson -------------------------------

import Data.Aeson  ( FromJSONKey )

-- base --------------------------------

import Data.Char  ( isAlpha, isAlphaNum )
import Data.List  ( intercalate )
import GHC.Exts   ( IsString( fromString ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char, digit, satisfy )
import Text.Parser.Combinators  ( count, optional, try, unexpected )

--------------------------------------------------------------------------------

{-| a nix (linux) architecture -}
newtype Arch = Arch { unArch ∷ 𝕋 }
  deriving newtype (Eq,FromJSONKey,IsString,Ord,Printable,Show)

{-| @Arch@ label for x86_64-linux -}
x86_64Linux ∷ Arch
x86_64Linux = "x86_64-linux"

------------------------------------------------------------

{-| a nix package hash -}
newtype Hash = Hash { unHash ∷ 𝕋 } deriving newtype (Eq,IsString,Printable,Show)

------------------------------------------------------------

{-| a nix package name -}
newtype Pkg  = Pkg  { unPkg  ∷ 𝕋 }
  deriving newtype (Eq,FromJSONKey,IsString,Ord,Printable,Show)

------------------------------------------------------------

{-| a nix package version -}
newtype Ver  = Ver  { unVer  ∷ 𝕋 } deriving newtype (Eq,IsString,Printable,Show)

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

-- that's all, folks! ----------------------------------------------------------
