{-# LANGUAGE UnicodeSyntax #-}
module Nix.Flake
  ( FlakePkg
  , FlakePkgs
  , flakePkgMap
  , flakePkgMap'
  , flakeShow
  , flakeShowNM
  , forMX86_64Pkg
  , forMX86_64Pkg_
  , forX86_64Pkg
  , location
  , namePkgVersPrioSrcArch
  , pkg
  , pkgFindNames
  , pkgFindNames'
  , priority
  , tests
  , ver
  , x86_64
  , x86_64_
  , x86_64_pkgs
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson ( eitherDecodeStrict' )

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail(fail) )
import Data.Function      ( flip )
import Data.Maybe         ( catMaybes, fromMaybe )
import Data.Tuple         ( uncurry )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Error.FPathError ( AsFPathError )
import FPath.File             ( File, FileAs )
import FPath.RelFile          ( relfile )

-- lens --------------------------------

import Control.Lens.At ( at )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Notice) )

-- mockio ------------------------------

import MockIO.DoMock ( DoMock(NoMock) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( HasDoMock, MockIOClass )

-- mockio-plus -------------------------

import MockIO.OpenFile ( readFileY )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, runReaderT )

-- parsers -----------------------------

import Text.Parser.Char ( char )

-- text --------------------------------

import Data.Text          ( intercalate )
import Data.Text.Encoding ( encodeUtf8 )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError )

-- tuple-plus --------------------------

import Data.TuplePlus ( (⨤), (⨦) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Error           ( AsNixDuplicatePkgError, AsNixError,
                             throwAsNixDuplicatePkgError,
                             throwAsNixErrorDuplicatePkg )
import Nix.NixExe          ( nixFlakeShow )
import Nix.Types           ( Arch, Pkg, Priority, RemoteState, unPriority,
                             x86_64Linux )
import Nix.Types.AttrPath  ( AttrPath, mkAttrPath )
import Nix.Types.ConfigDir ( ConfigDir(unConfigDir) )
import Nix.Types.FlakePkgs ( FlakePkg, FlakePkgs, FlakePkgs'(FlakePkgs'),
                             archMap, flakeShowTestInput, flakeShowTestMap,
                             location, packages, pkg, priority, ver )

--------------------------------------------------------------------------------

updatePriorities ∷ PkgPriorities → FlakePkgs → FlakePkgs
updatePriorities (PkgPriorities pps) fps =
  let go ∷ (Pkg,Priority) → FlakePkgs → FlakePkgs
      go (p,y) fpkgs = fpkgs & packages ⊧ Map.map (Map.adjust(& priority ⊩ y) p)
  in  foldr go fps (Map.toList pps)

----------------------------------------

locFile ∷ FlakePkgs → AbsFile
locFile fp = (unConfigDir $ fp ⊣ location) ⫻ [relfile|flake.nix|]

----------------------------------------

priosFile ∷ ConfigDir → AbsFile
priosFile fp = (unConfigDir fp) ⫻ [relfile|flake.priorities|]

----------------------------------------

x86_64 ∷ FlakePkgs → 𝕄 (Map.Map Pkg FlakePkg)
x86_64 f = (f ⊣ packages) ⊣ at x86_64Linux

x86_64_ ∷ Lens' FlakePkgs (Map.Map Pkg FlakePkg)
x86_64_ = go x86_64Linux
  where go ∷ Arch → Lens' FlakePkgs (Map.Map Pkg FlakePkg)
        go a =
          let f1 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg
              f1 fps = fromMaybe Map.empty $ a `Map.lookup` fps
              f2 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg
                 → Map.Map Arch (Map.Map Pkg FlakePkg)
              f2 fps new = Map.insert a new fps
          in  packages ∘ (lens f1 f2)

----------------------------------------

{-| apply a function to each named `FlakePkg` in x86_64-linux packages -}
forX86_64Pkg ∷ FlakePkgs → (Pkg → FlakePkg → α) → [α]
forX86_64Pkg fps f = case x86_64 fps of
  𝕵 pkg_map → (uncurry f) ⊳ (Map.toList pkg_map)
  𝕹         → []

----------------------------------------

{-| monadic apply a function to each named `FlakePkg` in x86_64-linux
    packages -}
forMX86_64Pkg ∷ Monad η ⇒ FlakePkgs → (Pkg → FlakePkg → η α) → η [α]
forMX86_64Pkg fps f = case x86_64 fps of
  𝕵 pkg_map → forM (Map.toList pkg_map) (uncurry f)
  𝕹         → return []

----------------------------------------

{-| monadic apply a function to each named `FlakePkg` in x86_64-linux
    packages; unify unit returns -}
forMX86_64Pkg_ ∷ Monad η ⇒ FlakePkgs → (Pkg → FlakePkg → η α) → η ()
forMX86_64Pkg_ fps f = forMX86_64Pkg fps f ⪼ return ()

----------------------------------------

pkgFind ∷ FlakePkgs → Pkg → [(Arch,FlakePkg)]
pkgFind fp p =
  catMaybes [ (a,) ⊳ p `Map.lookup` m | (a,m) ← Map.toList (fp ⊣ packages) ]

----------------------------------------

pkgName ∷ (Arch,FlakePkg) → (AttrPath, 𝕄 Priority)
pkgName (arch,fp) = (mkAttrPath (fp ⊣ pkg) ["packages", (toText arch)],
                     fp⊣priority)

----------------------------------------

pkgFindName_ ∷ ∀ ε η . (MonadError ε η) ⇒
               (Pkg → AbsFile → η (𝕄 (AttrPath,𝕄 Priority))) → FlakePkgs → Pkg
             → η (𝕄 (AttrPath, 𝕄 Priority))
pkgFindName_ t fp p = case pkgFind fp p of
                     []    → return 𝕹
                     [afp] → return $ 𝕵 (pkgName afp)
                     _     → t p (locFile fp)

--------------------

pkgFindName ∷ (AsNixDuplicatePkgError ε, MonadError ε η) ⇒
              FlakePkgs → Pkg → η (𝕄 (AttrPath, 𝕄 Priority))
pkgFindName = pkgFindName_ throwAsNixDuplicatePkgError

--------------------

pkgFindName' ∷ (AsNixError ε, MonadError ε η) ⇒
               FlakePkgs → Pkg → η (𝕄 (AttrPath, 𝕄 Priority))
pkgFindName' = pkgFindName_ throwAsNixErrorDuplicatePkg

----------------------------------------

pkgFindNames_ ∷ (Traversable ψ, MonadError ε η) ⇒
                (FlakePkgs → Pkg → η (𝕄 (AttrPath, (𝕄 Priority))))
              → FlakePkgs → ψ Pkg → η (ψ (Pkg, 𝕄 (AttrPath, (𝕄 Priority))))
pkgFindNames_ f fp = mapM (\ p → (p,) ⊳ f fp p)

pkgFindNames ∷ (Traversable ψ, AsNixDuplicatePkgError ε, MonadError ε η) ⇒
               FlakePkgs → ψ Pkg → η (ψ (Pkg, 𝕄 (AttrPath, (𝕄 Priority))))
pkgFindNames = pkgFindNames_ pkgFindName

pkgFindNames' ∷ (Traversable ψ, AsNixError ε, MonadError ε η) ⇒
                FlakePkgs → ψ Pkg → η (ψ (Pkg, 𝕄 (AttrPath, (𝕄 Priority))))
pkgFindNames' = pkgFindNames_ pkgFindName'

----------------------------------------

{-| nix flake show #flake -}
flakeShow ∷ ∀ ε δ μ .
            (MonadIO μ, HasDoMock δ, MonadReader δ μ,
             AsIOError ε, AsFPathError ε, AsCreateProcError ε,
             AsTextualParseError ε, AsProcExitError ε, AsAesonError ε,
             Printable ε, MonadError ε μ,
             MonadLog (Log MockIOClass) μ) ⇒
            RemoteState → ConfigDir → μ FlakePkgs
flakeShow r d = do
  flake_pkgs ← nixFlakeShow r d
  prios ← readPriorities (priosFile d)
  return (updatePriorities prios flake_pkgs)

----------------------------------------

{-| `flakeShowNM`, never mock -}
flakeShowNM ∷ ∀ ε μ .
              (MonadIO μ,
               AsIOError ε, AsFPathError ε, AsCreateProcError ε,
               AsProcExitError ε, AsAesonError ε, AsTextualParseError ε,
               Printable ε, MonadError ε μ,
               MonadLog (Log MockIOClass) μ) ⇒
              RemoteState → ConfigDir → μ FlakePkgs
flakeShowNM r = flip runReaderT NoMock ∘ flakeShow r

----------------------------------------

flakeDecodeTests ∷ TestTree
flakeDecodeTests =
  testCase "flakeDecode" $
    𝕽 (FlakePkgs' $ Map.fromList [("x86_64-linux",flakeShowTestMap)] ) @=?
      eitherDecodeStrict' (encodeUtf8 flakeShowTestInput)

----------------------------------------

{-| Convert to a Map from `Pkg` to full addressable name in the flake, e.g.,
    "packages.x86_64-linux.cabal" -}
flakePkgMap_ ∷ ∀ ε η . MonadError ε η ⇒
              (∀ ω . Pkg → AbsFile → η ω) → FlakePkgs → η (Map.Map Pkg 𝕋)
flakePkgMap_ throw fp =
  let throwDup k _ _ = throw k (locFile fp)
  in  sequence $ Map.fromListWithKey throwDup
        [ (p,return $ [fmt|packages.%T.%T|] arch p)
        | (arch,pkgs) ← Map.toList (fp ⊣ packages), (p,_) ← Map.toList pkgs ]

flakePkgMap ∷ ∀ ε η . (AsNixDuplicatePkgError ε, MonadError ε η) ⇒
              FlakePkgs → η (Map.Map Pkg 𝕋)
flakePkgMap = flakePkgMap_ throwAsNixDuplicatePkgError

flakePkgMap' ∷ ∀ ε η . (AsNixError ε, MonadError ε η) ⇒
               FlakePkgs → η (Map.Map Pkg 𝕋)
flakePkgMap' = flakePkgMap_ throwAsNixErrorDuplicatePkg

newtype PkgPriority = PkgPriority { unPkgPriority :: (Pkg, Priority) }
  deriving (Show)

instance Printable PkgPriority where
  print (PkgPriority (k,r)) = P.text $ [fmt|%T:%d|] k (unPriority r)

instance TextualPlus PkgPriority where
  textual' = let tabs = some $ char '\t'
             in  PkgPriority ⊳ (((,) ⊳ textual' ⋪ tabs ⊵ textual'))

newtype PkgPriorities = PkgPriorities (Map.Map Pkg Priority)
  deriving (Show)

pkgPrioritiesFromList ∷ MonadFail η ⇒ [PkgPriority] → η PkgPriorities
pkgPrioritiesFromList pkps =
  let go ∷ MonadFail η ⇒
           Map.Map Pkg Priority → PkgPriority → η (Map.Map Pkg Priority)
      go pps (PkgPriority (p,y)) =
        case p `Map.lookup` pps of
          𝕹 → return $ Map.insert p y pps
          𝕵 y' → fail $ [fmt|duplicate priorities found for %T: (%T,%T)|] p y y'
  in  PkgPriorities ⊳ foldM go Map.empty pkps
{-
  let xx ∷ PkgPriority → (Pkg,[Priority])
      xx = _
      -- A map from Pkg to all the priorities it's associated with
      -- (including duplicates)
      proto_map ∷ Map.Map Pkg [Priority]
      proto_map = Map.fromListWith _ (xx ⊳ pkps)
      go pkg [prio] accum =
        case accum of
          𝕷 errs → errs
          𝕽 accum' → case pkg `Map.lookup` accum' of
                       𝕹   → 𝕽 (_ ∷ Map.Map Pkg Priority)
                       𝕵 _ → 𝕷 (_ ∷ Map.Map Pkg [Priority])
      go pkg prios accum =
        case accum of
          𝕷 errs → _
          𝕽 accum' → case pkg `Map.lookup` accum' of
                       𝕹   → 𝕽 (_ ∷ Map.Map Pkg Priority)
                       𝕵 _ → 𝕷 (_ ∷ Map.Map Pkg [Priority])
-- XXX could this be foldMapWithKey?
  in case Map.foldrWithKey go (return $ Map.empty) proto_map of
    𝕷 e → fail $ _ e
    𝕽 r → return $ PkgPriorities r
-}

instance Printable PkgPriorities where
  print (PkgPriorities pps) =
    P.text ∘ intercalate "\n" $ toText ∘ PkgPriority ⊳ Map.toList pps

instance TextualPlus PkgPriorities where
  -- textual' = pkgPrioritiesFromList ⊳ many (textual' ⋪ char '\n')
  textual' = many (textual' ⋪ char '\n') ≫ pkgPrioritiesFromList

readPriorities ∷ ∀ ε γ ω μ .
                 (HasDoMock ω, HasIOClass ω,
                  Default ω, MonadLog (Log ω) μ, MonadError ε μ, AsIOError ε,
               AsTextualParseError ε,
                  FileAs γ, MonadIO μ, Printable ε) ⇒
                 γ → μ PkgPriorities

readPriorities f =
  let fmsg ∷ 𝕄 (File → 𝕋)
      fmsg = 𝕵 [fmt|reading priorities: %T|]
  in  readFileY @_ @𝕋 Notice fmsg ф f NoMock ≫ tparse ∘ fromMaybe ""

----------------------------------------

x86_64_pkgs ∷ FlakePkgs → [Pkg]
x86_64_pkgs fp = case x86_64 fp of
                   𝕹   → []
                   𝕵 m → Map.keys m

----------------------------------------

namePkgVersPrioSrcArch ∷ FlakePkgs → [(𝕋,𝕋,𝕋,𝕋,𝕋,𝕋)]
namePkgVersPrioSrcArch pkgs =
  let
    pkg_ver ∷ FlakePkg → (𝕋,𝕋,𝕋)
    pkg_ver fp = (toText $ fp ⊣ pkg, maybe "" toText $ fp ⊣ ver,
                  maybe "" toText $ fp ⊣ priority)

    go ∷ Pkg → FlakePkg → [(𝕋,𝕋,𝕋,𝕋,𝕋)]
    go p fp = [(toText p ⨤ (pkg_ver fp) ∷ (𝕋,𝕋,𝕋,𝕋)) ⨦ toText (pkgs ⊣ location)]
    go' ∷ Arch → Map.Map Pkg FlakePkg → [(𝕋,𝕋,𝕋,𝕋,𝕋,𝕋)]
    go' arch fpmap = (⨦ (toText arch)) ⊳ Map.foldMapWithKey go fpmap
  in
    Map.foldMapWithKey go' (pkgs ⊣ archMap)

-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Nix.Flake"
    [ flakeDecodeTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
