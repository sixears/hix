{-# LANGUAGE DerivingStrategies #-}

{-| miscellaneous small types used in nix profile representation -}
module Nix.Profile.Types
  ( Hash( unHash ), Pkg( unPkg ), Ver( unVer ) )
where

import Base1T

-- base --------------------------------

import GHC.Exts  ( IsString )

--------------------------------------------------------------------------------

{-| a nix package hash -}
newtype Hash = Hash { unHash ∷ 𝕋 } deriving newtype (Eq,IsString,Printable,Show)
{-| a nix package name -}
newtype Pkg  = Pkg  { unPkg  ∷ 𝕋 } deriving newtype (Eq,IsString,Printable,Show)
{-| a nix package version -}
newtype Ver  = Ver  { unVer  ∷ 𝕋 } deriving newtype (Eq,IsString,Printable,Show)

-- that's all, folks! ----------------------------------------------------------
