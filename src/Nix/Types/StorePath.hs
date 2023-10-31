{-# LANGUAGE UnicodeSyntax #-}
{-| A path into the nix store, with metadata; e.g.,

>    /nix/store/g9zcvd6f5aasrxwm48bdbks3scv46b6x-jq-1.6-bin
>    /nix/store/m479892zj5kcgnvv1cpxsqnawyn4ai99-jq-1.6-man

 -}
module Nix.Types.StorePath
  ( StorePath
  , spPkgVerPath
  , tests
  ) where

import Base1T

-- base --------------------------------

import Control.Monad.Fail ( MonadFail )
import Data.Char          ( isAlpha, isAlphaNum )
import GHC.Exts           ( fromString )

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, absdir, parseAbsDirP )
import FPath.AppendableFPath  ( (⫻) )
import FPath.AsFilePath'      ( filepath' )
import FPath.Error.FPathError ( FPathError )
import FPath.RelDir           ( reldir )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, alphaNum, char, digit, satisfy,
                                 string )
import Text.Parser.Combinators ( count, optional, try, unexpected )

-- text --------------------------------

import Data.Text ( pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), checkT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types ( Hash(unHash), Pkg(unPkg), Ver(unVer), pkgRE )

--------------------------------------------------------------------------------

{-| A path into the nix store, with metadata; e.g.,

>    /nix/store/g9zcvd6f5aasrxwm48bdbks3scv46b6x-jq-1.6-bin
>    /nix/store/m479892zj5kcgnvv1cpxsqnawyn4ai99-jq-1.6-man

 -}
data StorePath = StorePath { _path' :: AbsDir
                           , _hash  :: Hash
                           , _pkg'  :: Pkg
                           , _ver   :: 𝕄 Ver
                           }
  deriving (Eq, Show)

{-| Match against a store path, e.g.,

    /nix/store/0dbkb5963hjgg45yw07sk3dm43jci4bw-atreus-1.0.2.0

   return hash, pkg, (maybe) ver
-}
storePathRE ∷ (CharParsing η, MonadFail η) ⇒ η (Hash, Pkg, 𝕄 Ver)
storePathRE =
  let
    _pkgRE ∷ CharParsing η ⇒ η (𝕊, 𝕄 𝕊)
    _pkgRE =
      let
        alpha_under_score      ∷ CharParsing η ⇒ η ℂ
        alpha_under_score      = satisfy (\ c → isAlpha c ∨ c ≡ '_')
        non_hyphen             ∷ CharParsing η ⇒ η ℂ
        non_hyphen             = satisfy (\ c → isAlphaNum c ∨ c ∈ "_.")
        simple_identifier      ∷ CharParsing η ⇒ η 𝕊
        simple_identifier      = (:) ⊳ alpha_under_score ⊵ many non_hyphen
        hyphenated_identifiers ∷ CharParsing η ⇒ η 𝕊
        hyphenated_identifiers =
          ю ⊳ ((:) ⊳ simple_identifier ⊵many(try $ char '-' ⋫simple_identifier))
        numeric_identifier     ∷ CharParsing η ⇒ η 𝕊
        numeric_identifier     =
          (:) ⊳ digit ⊵ many (satisfy (\ c → isAlphaNum c ∨ c ∈ "-_."))
      in
        ((,) ⊳ hyphenated_identifiers ⊵ optional(char '-' ⋫ numeric_identifier))
  in
    (\ h (p,v) → (fromString h, p, v)) ⊳
      (string "/nix/store/" ⋫ count 32 alphaNum) ⊵ (char '-' ⋫ pkgRE)

instance Printable StorePath where
  print (StorePath _ h p v) =
    let v' = case v of 𝕹 → ""; 𝕵 v_ → "-" ⊕ unVer v_
    in  P.text $ [fmt|/nix/store/%T-%T-%T/|] h p v'

instance TextualPlus StorePath where
  textual' = do
    let construct p h v =
          either (unexpected ∘ toString) pure $
            parseAbsDirP @FPathError @_ @(𝔼 _) $
              ю [ "/nix/store/"
                 , (unHash h)
                 , "-", (unPkg p)
                 , maybe "" ("-" ⊕) ((unVer) ⊳ v)
                 ]
    (h,p,v) ← storePathRE
    t ← construct p h v
    return (StorePath t h p v)

{-| extract package name, version, and absolute dir from a @StorePath@ -}
spPkgVerPath ∷ StorePath → (Pkg, 𝕄 Ver, AbsDir)
spPkgVerPath sp = (_pkg' sp, _ver sp, _path' sp)

-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "storePath"
    [ let
        hash          = "0dbkb5963hjgg45yw07sk3dm43jci4bw"
        dirname       =
          [reldir|0dbkb5963hjgg45yw07sk3dm43jci4bw-atreus-1.0.2.0/|]
        path ∷ AbsDir = [absdir|/nix/store/|] ⫻ dirname
        path'         = pack $ path ⫥ filepath'
      in
        checkT path' (StorePath { _path' = path
                                , _hash  = hash
                                , _pkg'  = "atreus"
                                , _ver   = 𝕵 "1.0.2.0"
                                })
    , let
        hash          = "g9zcvd6f5aasrxwm48bdbks3scv46b6x"
        dirname       =
          [reldir|g9zcvd6f5aasrxwm48bdbks3scv46b6x-jq-1.6-bin/|]
        path ∷ AbsDir = [absdir|/nix/store/|] ⫻ dirname
        path'         = pack $ path ⫥ filepath'
      in
        checkT path' (StorePath { _path' = path
                                , _hash  = hash
                                , _pkg'  = "jq"
                                -- the use of -bin in the version is
                                -- unsatisfying; but I can't see how to
                                -- distinguish from e.g., bash-5.1-p16, where
                                -- p16 *is* part of the version
                                , _ver   = 𝕵 "1.6-bin" })
    ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
