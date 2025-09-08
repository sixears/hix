{-# LANGUAGE UnicodeSyntax #-}
module Nix.T.Helpers
  ( checkFromJSON
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson ( FromJSON, eitherDecodeStrict' )

-- bytestring --------------------------

import Data.ByteString qualified as BS

--------------------------------------------------------------------------------

checkFromJSON ∷ (FromJSON α, Eq α, Show α) ⇒ 𝕊 → BS.ByteString → α → TestTree
checkFromJSON name input exp =
  testCase name $ 𝓡 exp @=? eitherDecodeStrict' input

-- that's all, folks! ----------------------------------------------------------
