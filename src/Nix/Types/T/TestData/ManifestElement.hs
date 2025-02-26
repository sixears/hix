{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.T.TestData.ManifestElement
  where

import Base1T

-- bytestring --------------------------

import Data.ByteString qualified as BS

-- data-textual ------------------------

import Data.Textual ( toUtf8 )

-- textual-plus ------------------------

import TextualPlus ( qquote )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types.AttrPath ( AttrPath, mkAttrPath )

--------------------------------------------------------------------------------

storePath1 ∷ 𝕋
storePath1 =
  "/nix/store/3xslcmxpm8zjavfiwz71q3p3b0zj1y3g-chrysalis-0.13.3-binary"

----------

storePath2 ∷ 𝕋
storePath2 =
  "/nix/store/dbnqbqypqrw3jq2rpzzr945zaicz6jyk-gqview-2.1.5"

--------------------

url1 ∷ 𝕋
url1 = "git+file:///home/martyn/nix/desktop"

----------

url2 ∷ 𝕋
url2 = "git+file:///home/martyn/nix/desktop"

--------------------

attrPath1 ∷ AttrPath
attrPath1 = mkAttrPath "chrysalis" ["packages","x86_64-linux"]

----------

attrPath2 ∷ AttrPath
attrPath2 = mkAttrPath "gqview" ["packages","x86_64-linux"]

--------------------

manifestElement1BS ∷ BS.ByteString
manifestElement1BS = ю
  [ "    {"
  , "      \"active\": true,"
  , "      \"attrPath\": ", toUtf8 ∘ qquote $ toText attrPath1, ","
  , "      \"originalUrl\": ", toUtf8 $ qquote url1, ","
  , "      \"outputs\": null,"
  , "      \"priority\": 5,"
  , "      \"storePaths\": [", toUtf8 $ qquote storePath1, "],"
  , "      \"url\": ", toUtf8 $ qquote url1
  , "    }"
  ]

manifestElement2BS ∷ BS.ByteString
manifestElement2BS = ю
  [ "    {"
  , "      \"active\": true,"
  , "      \"attrPath\": ", toUtf8 ∘ qquote $ toText attrPath2, ","
  , "      \"originalUrl\": ", toUtf8 $ qquote url2, ","
  , "      \"outputs\": null,"
  , "      \"priority\": 3,"
  , "      \"storePaths\": [", toUtf8 $ qquote storePath2, "],"
  , "      \"url\": ", toUtf8 $ qquote url2
  , "    }"
  ]

--------------------------------------------------------------------------------
