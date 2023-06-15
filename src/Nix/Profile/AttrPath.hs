{-| A nix module attr path (e.g., `packages.x86_64-linux.jq`).
    A single @AttrPath@ may relate to multiple @StorePaths@, e.g., `-bin`,
    `-man`.
-}
module Nix.Profile.AttrPath
  ( apPkg, tests )
where

import Base1T

-- base --------------------------------

import qualified  Data.List.NonEmpty  as  NonEmpty

import Data.List  ( reverse )
import GHC.Exts   ( fromString )

-- parsers -----------------------------

import Text.Parser.Char         ( char, noneOf )
import Text.Parser.Combinators  ( sepByNonEmpty )

-- text --------------------------------

import Data.Text  ( intercalate, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus ------------------------

import TextualPlus  ( ParseableInput, TextualPlus( textual' )
                    , parseText, tparse )
import TextualPlus.Error.TextualParseError
                    ( AsTextualParseError, tparseToME' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Profile.Types  ( Pkg( unPkg ) )

--------------------------------------------------------------------------------

data AttrPath = AttrPath { _attrPrefixParts ∷ [𝕋], _pkg ∷ Pkg }
  deriving (Eq,Show)

instance Printable AttrPath where
  print (AttrPath ps p) = P.text $ intercalate "." (ps ⊕ [unPkg p])

instance TextualPlus AttrPath where
  textual' =
    let mkAttrPath ∷ NonEmpty 𝕊 → AttrPath
        mkAttrPath (x :| xs) = (AttrPath (pack ⊳ reverse xs) (fromString x))
        mkAttrPath' ∷ NonEmpty 𝕊 → AttrPath
        mkAttrPath' = mkAttrPath ∘ NonEmpty.reverse
    in  mkAttrPath' ⊳ sepByNonEmpty (some (noneOf ".")) (char '.')

checkT ∷ (TextualPlus α, Eq α, Show α) ⇒ 𝕋 → α → TestTree
checkT input exp =
  testCase ("parseText: " ⊕ unpack input) $
    𝕽 exp @=? (tparseToME' ∘ parseText) input

{-| parse an attrpath, return the pkg -}
apPkg ∷ (ParseableInput α, AsTextualParseError ε, MonadError ε η) ⇒ α → η Pkg
apPkg ap = (_pkg ⊳ tparse ap)

-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "attrPath"
    [ checkT "packages.x86_64-linux.pia"
            (AttrPath ["packages","x86_64-linux"] "pia")
    , checkT "packages.x86_64-linux.nix-prefetch-github"
            (AttrPath ["packages","x86_64-linux"] "nix-prefetch-github")
    ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
