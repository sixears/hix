{-# LANGUAGE UnicodeSyntax #-}
{-| Full collection of tests for `containers-plus` -}

module Nix.T.Nix
  ( tests
  ) where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Flake                 qualified as Flake
import Nix.Types.AttrPath        qualified as AttrPath
import Nix.Types.Manifest        qualified as Manifest
import Nix.Types.ManifestElement qualified as ManifestElement
import Nix.Types.StorePath       qualified as StorePath

--------------------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = testGroup "Nix" [ AttrPath.tests, StorePath.tests, ManifestElement.tests
                        , Manifest.tests, Flake.tests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

-- that's all, folks! ----------------------------------------------------------
