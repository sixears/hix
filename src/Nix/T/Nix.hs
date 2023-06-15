{-| Full collection of tests for `containers-plus` -}

module Nix.T.Nix
  ( tests )
where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Nix.Profile.AttrPath   as  AttrPath
import qualified  Nix.Profile.StorePath  as  StorePath

--------------------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = testGroup "Nix" [ AttrPath.tests, StorePath.tests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

-- that's all, folks! ----------------------------------------------------------
