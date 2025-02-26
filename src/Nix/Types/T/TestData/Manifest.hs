{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.T.TestData.Manifest
  where

import Base1T

-- bytestring --------------------------

import Data.ByteString qualified as BS

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types.T.TestData.ManifestElement qualified as ManifestElement

--------------------------------------------------------------------------------

v2Manifest ∷ BS.ByteString
v2Manifest = ю
  [ "{"
  , "  \"elements\": ["
  , ManifestElement.manifestElement1BS
  , ","
  , ManifestElement.manifestElement2BS
  , "  ],"
  , "  \"version\": 2"
  , "}"
  ]

--------------------

v3Manifest ∷ BS.ByteString
v3Manifest = ю
  [ "{"
  , "  \"elements\": {"
  , "    \"chrysalis\": ", ManifestElement.manifestElement1BS
  , "    ,"
  , "    \"gqview\": ", ManifestElement.manifestElement2BS
  , "  },"
  , "  \"version\": 3"
  , "}"
  ]

-- that's all, folks! ----------------------------------------------------------
