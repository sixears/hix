{-# LANGUAGE UnicodeSyntax #-}
{-| A nix module attr path (e.g., `packages.x86_64-linux.jq`).
    A single @AttrPath@ may relate to multiple @StorePaths@, e.g., `-bin`,
    `-man`.
-}
module Nix.Types.AttrPath
  ( AttrPath
  , apPkg
  , mkAttrPath
  , pkg
  , tests
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson       ( FromJSON(parseJSON), Value(String) )
import Data.Aeson.Types ( typeMismatch )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Monad.Fail ( fail )
import Data.List          ( reverse )
import Data.Ord           ( Ord(compare), comparing )
import GHC.Exts           ( fromString )

-- data-textual ------------------------

import Data.Textual ( Parsed(Malformed, Parsed) )

-- parsers -----------------------------

import Text.Parser.Char        ( char, noneOf )
import Text.Parser.Combinators ( sepByNonEmpty )

-- text --------------------------------

import Data.Text ( intercalate, pack, unpack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( ParseableInput,
                                             TextualPlus(textual'), parseText,
                                             tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError, tparseToME' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types ( Pkg(unPkg) )

--------------------------------------------------------------------------------

data AttrPath = AttrPath { _attrPrefixParts :: [𝕋]
                         , _pkg             :: Pkg
                         }
  deriving (Eq, Show)

pkg ∷ Lens' AttrPath Pkg
pkg = lens _pkg (\ ap p → ap { _pkg = p })

instance Printable AttrPath where
  print (AttrPath ps p) = P.text $ intercalate "." (ps ⊕ [unPkg p])

instance Ord AttrPath where
  compare = comparing toText

instance TextualPlus AttrPath where
  textual' =
    let mkAttrPath_ ∷ NonEmpty 𝕊 → AttrPath
        mkAttrPath_ (x :| xs) = AttrPath (pack ⊳ reverse xs) (fromString x)
        mkAttrPath' ∷ NonEmpty 𝕊 → AttrPath
        mkAttrPath' = mkAttrPath_ ∘ NonEmpty.reverse
    in  mkAttrPath' ⊳ sepByNonEmpty (some (noneOf ".")) (char '.')

instance FromJSON AttrPath where
  parseJSON (String s) = case parseText s of
    Malformed es e → fail $ [fmt|%L ⫽ %s|] es e
    Parsed t       → return t
  parseJSON invalid = typeMismatch "AttrPath" invalid

checkT ∷ (TextualPlus α, Eq α, Show α) ⇒ 𝕋 → α → TestTree
checkT input exp =
  testCase ("parseText: " ⊕ unpack input) $
    𝕽 exp @=? (tparseToME' ∘ parseText) input

----------------------------------------

{-| parse an attrpath, return the pkg -}
apPkg ∷ (ParseableInput α, AsTextualParseError ε, MonadError ε η) ⇒ α → η Pkg
apPkg ap = (_pkg ⊳ tparse ap)

----------------------------------------

{-| Create an `AttrPath` from a `Pkg` and a list of prefix strings
  mkAttrPath (Pkg "emacs") ["packages","x86_64-linux"] -> "packages.x86_64-linux.emacs"
-}
mkAttrPath ∷ Pkg → [𝕋] → AttrPath
mkAttrPath p ts = AttrPath ts p

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
