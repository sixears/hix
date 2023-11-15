{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.ToBriefText
  ( ToBriefText(..)
  ) where

import Base1T

--------------------------------------------------------------------------------

class ToBriefText α where
  toT ∷ α → 𝕋

instance ToBriefText 𝕋 where
  toT = id

instance (ToBriefText α, ToBriefText β) ⇒ ToBriefText (α,β) where
  toT (a,b) =
    let a' = toT a
        b' = toT b
    in  if a' ≡ b' then a' else [fmt|%t→%t|] (toT a) (toT b)

-- that's all, folks! ----------------------------------------------------------

