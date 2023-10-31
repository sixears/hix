{-# LANGUAGE UnicodeSyntax #-}
module Nix.Flake
  ( FlakePkg
  , FlakePkgs
  , HasArchFlakePkgMap(archMap)
  , flakePkgMap
  , flakePkgMap'
  , flakeShow
  , flakeShowNM
  , forMX86_64Pkg
  , forMX86_64Pkg_
  , forX86_64Pkg
  , location
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


import Prelude ( undefined )

-- aeson -------------------------------

import Data.Aeson ( eitherDecodeStrict' )

import Data.Aeson.Types as AesonTypes

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError, throwAsAesonError )

-- base --------------------------------

import Control.Monad      ( foldM )
import Control.Monad.Fail ( MonadFail(fail) )
import Data.Function      ( flip )
import Data.Maybe         ( catMaybes, fromMaybe )
import Data.Monoid        ( Monoid )
import Data.Tuple         ( uncurry )
import GHC.Generics       ( Generic )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.AsFilePath       ( filepath )
import FPath.Error.FPathError ( AsFPathError )
import FPath.File             ( File, FileAs(_File_) )
import FPath.RelFile          ( relfile )

-- lens --------------------------------

import Control.Lens.At     ( at )
import Control.Lens.Getter ( view )
import Control.Lens.Tuple  ( _2 )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Notice) )

-- mockio ------------------------------

import MockIO.DoMock ( DoMock(NoMock) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass, IOClass(IORead, IOWrite) )
import MockIO.Log     ( HasDoMock, MockIOClass )

-- mockio-plus -------------------------

import MockIO.OpenFile          ( readFileY )
import MockIO.Process.MLCmdSpec ( MLCmdSpec, mock_value )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.NamedHandle           ( HGetContents(hGetContents),
                                       HWriteContents(hWriteContents), ℍ,
                                       impliedEncoding, impliedEncodingM )
import MonadIO.Process.ExitStatus    ( ExitStatus, evOK )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, runReaderT )

-- parsers -----------------------------

import Text.Parser.Char ( char )

-- text --------------------------------

import Data.Text          ( concat, intercalate, pack, unpack )
import Data.Text.Encoding ( encodeUtf8 )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( PrintOut, TextualPlus(textual'),
                                             parseT, parseTextual, tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix                ( nixDo )
import Nix.Error          ( AsNixDuplicatePkgError, AsNixError,
                            throwAsNixDuplicatePkgError,
                            throwAsNixErrorDuplicatePkg )
import Nix.Types          ( Arch, ConfigDir(unConfigDir), Pkg, Priority,
                            RemoteState, Ver, pkgRE, remoteArgs, unPriority,
                            x86_64Linux )
import Nix.Types.AttrPath ( AttrPath, mkAttrPath )

--------------------------------------------------------------------------------

data FlakePkg = FlakePkg { _description :: 𝕄 𝕋
                         , _pkg         :: Pkg
                         , _ver         :: 𝕄 Ver
                         , _type        :: 𝕋
                           -- priority isn't really in the flake, it's in our
                           -- own flake.priorities
                         , _priority    :: 𝕄 Priority
                         }
  deriving (Eq, Generic, Show)

pkg ∷ Lens' FlakePkg Pkg
pkg = lens _pkg (\ fp p → (fp { _pkg = p }))

ver ∷ Lens' FlakePkg (𝕄 Ver)
ver = lens _ver (\ fp v → (fp { _ver = v }))

priority ∷ Lens' FlakePkg (𝕄 Priority)
priority = lens _priority (\ fp p → (fp { _priority = p }))

pkgVer ∷ FlakePkg → 𝕋
pkgVer f =
  let p = toText $ f ⊣ pkg
  in  case f ⊣ ver of
        𝕹   → p
        𝕵 v → [fmt|%t-%T|] p v

instance FromJSON FlakePkg where
  parseJSON =
    withObject "FlakePkg" $
    \ v → do
          name ← v .: "name"
          (p,vers) ← parseT pkgRE "FlakePkg" (unpack name)
          FlakePkg ⊳ v .:? "description" ⊵ return p ⊵ return vers ⊵ v .: "type"
                   -- when reading the flake show output, priority is always 𝕹
                   -- as we read this from flake.priorities
                   ⊵ pure 𝕹
instance Printable FlakePkg where
  print = P.text ∘ pkgVer

------------------------------------------------------------

type Map = Map.Map

newtype FlakePkgs' = FlakePkgs' { unFlakePkgs' :: Map Arch (Map Pkg FlakePkg) }
  deriving (Eq, Generic, Show)

instance FromJSON FlakePkgs' where
  parseJSON =
    withObject "FlakePkgs'" $ \ v →
    {- let customOptions ∷ AesonTypes.Options
        customOptions =
          let dropLeadingUnderscore ('_' : s) = s
              dropLeadingUnderscore s         = s
          in  defaultOptions { fieldLabelModifier = dropLeadingUnderscore }
    in -} FlakePkgs' ⊳ v .: "packages"


class HasArchFlakePkgMap α where
  archMap ∷ Lens' α (Map Arch (Map Pkg FlakePkg))

instance HasArchFlakePkgMap (Map Arch (Map Pkg FlakePkg)) where
  archMap = id

instance HasArchFlakePkgMap FlakePkgs' where
  archMap = lens unFlakePkgs' (\ _ m → FlakePkgs' m)

updatePriorities' ∷ PkgPriorities → FlakePkgs' → FlakePkgs'
updatePriorities' pkgprios (FlakePkgs' fps) =
  FlakePkgs' (Map.map (Map.map undefined) fps)

--------------------

data FlakePkgs = FlakePkgs { _location :: ConfigDir
                           , _packages :: FlakePkgs'
                           }
  deriving (Show)

location ∷ Lens' FlakePkgs ConfigDir
location = lens _location (\ fp l → fp { _location = l })

packages ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
packages = lens (unFlakePkgs' ∘ _packages)
                (\ p f → (p { _packages = FlakePkgs' f }))

instance HasArchFlakePkgMap FlakePkgs where
  archMap = packages ∘ archMap

instance Printable FlakePkgs where
  print fp =
    let namePkg ∷ Arch → (Map.Map Pkg FlakePkg) → [𝕋]
        namePkg arch pkgs = [ [fmt|packages.%T.%T|] arch p
                              | (p,_) ← Map.toList pkgs ]
    in P.text $
    intercalate " ⫽ " [ intercalate "," (namePkg arch pkgs)
                        | (arch,pkgs) ← Map.toList (fp ⊣ packages) ]

----------------------------------------

updatePriorities ∷ PkgPriorities → FlakePkgs → FlakePkgs
updatePriorities (PkgPriorities pps) fps =
  let go ∷ (Pkg,Priority) → FlakePkgs → FlakePkgs
      go (p,y) fpkgs = fpkgs & packages ⊧ Map.map (Map.adjust(& priority ⊩ y) p)
  in  foldr go fps (Map.toList pps)

----------------------------------------

locFile ∷ FlakePkgs → AbsFile
locFile fp = (unConfigDir $ _location fp) ⫻ [relfile|flake.nix|]

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
          let pkgs ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
              pkgs = lens (unFlakePkgs' ∘ _packages)
                          (\ p f → (p { _packages = FlakePkgs' f }))
              f1 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg
              f1 fps = fromMaybe Map.empty $ a `Map.lookup` fps
              f2 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg
                 → Map.Map Arch (Map.Map Pkg FlakePkg)
              f2 fps new = Map.insert a new fps
          in  pkgs ∘ (lens f1 f2)

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

pkgName ∷ (Arch,FlakePkg) → AttrPath
pkgName (arch,fp) = mkAttrPath (fp ⊣ pkg) ["packages", (toText arch)]

----------------------------------------

pkgFindName_ ∷ (MonadError ε η) ⇒
               (Pkg → AbsFile → η (𝕄 AttrPath)) → FlakePkgs → Pkg
             → η (𝕄 AttrPath)
pkgFindName_ t fp p = case pkgFind fp p of
                     []    → return 𝕹
                     [afp] → return $ 𝕵 (pkgName afp)
                     _     → t p (locFile fp)

--------------------

pkgFindName ∷ (AsNixDuplicatePkgError ε, MonadError ε η) ⇒
              FlakePkgs → Pkg → η (𝕄 AttrPath)
pkgFindName = pkgFindName_ throwAsNixDuplicatePkgError

--------------------

pkgFindName' ∷ (AsNixError ε, MonadError ε η) ⇒ FlakePkgs → Pkg → η (𝕄 AttrPath)
pkgFindName' = pkgFindName_ throwAsNixErrorDuplicatePkg

----------------------------------------

pkgFindNames_ ∷ (Traversable ψ, MonadError ε η) ⇒
                (FlakePkgs → Pkg → η (𝕄 AttrPath)) → FlakePkgs → ψ Pkg
              → η (ψ (Pkg, 𝕄 AttrPath))
pkgFindNames_ f fp = mapM (\ p → (p,) ⊳ f fp p)

pkgFindNames ∷ (Traversable ψ, AsNixDuplicatePkgError ε, MonadError ε η) ⇒
               FlakePkgs → ψ Pkg → η (ψ (Pkg, 𝕄 AttrPath))
pkgFindNames = pkgFindNames_ pkgFindName

pkgFindNames' ∷ (Traversable ψ, AsNixError ε, MonadError ε η) ⇒
                FlakePkgs → ψ Pkg → η (ψ (Pkg, 𝕄 AttrPath))
pkgFindNames' = pkgFindNames_ pkgFindName'

----------------------------------------

flakeShowTestInput ∷ 𝕋
flakeShowTestInput =
  concat [ "{ \"packages\": {"
         , "    \"x86_64-linux\": {"

         , "      \"binutils\": {"
         , "        \"description\": \"MOCK MOCK MOCK\","
         , "        \"name\": \"binutils-wrapper-2.38\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"get-iplayer-config\": {"
         , "        \"name\": \"get-iplayer-config\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"graph-easy\": {"
         , "        \"description\": \"MOCK MOCKETY MOCK\","
         , "        \"name\": \"perl5.34.1-Graph-Easy-0.76\","
         , "        \"type\": \"derivation\""
         , "      }"

         , "    }"
         , "  }"
         , "}"
         ]

--------------------

flakeShowTestMap ∷ Map.Map Pkg FlakePkg
flakeShowTestMap = fromList [ ("binutils",
                               FlakePkg { _description = 𝕵 "MOCK MOCK MOCK"
                                        , _pkg = "binutils-wrapper"
                                        , _ver = Just "2.38"
                                        , _type = "derivation"
                                        , _priority = 𝕹
                                        })
                            , ("get-iplayer-config",
                               FlakePkg { _description = 𝕹
                                        , _pkg = "get-iplayer-config"
                                        , _ver = 𝕹
                                        , _type = "derivation"
                                        , _priority = 𝕹
                                        })
                            , ("graph-easy",
                               FlakePkg { _description = 𝕵 "MOCK MOCKETY MOCK"
                                        , _pkg = "perl5.34.1-Graph-Easy"
                                        , _ver = 𝕵 "0.76"
                                        , _type = "derivation"
                                        , _priority = 𝕹
                                        })
                            ]

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
  let eAsAesonError ∷ (Printable τ,AsAesonError ε,MonadError ε η) ⇒ 𝔼 τ β → η β
      eAsAesonError = either throwAsAesonError return
      mock_set ∷ MLCmdSpec 𝕋 → MLCmdSpec 𝕋
      mock_set = let mock_val ∷ (ExitStatus, 𝕋) = (evOK, flakeShowTestInput)
                 in  (& mock_value ⊢ mock_val)
      args     = ю [ ["flake", "show", "--json" ]
                   , remoteArgs r
                   , [ pack $ (unConfigDir d) ⫥ filepath ] ]
--  (_,flake_show) ← ꙩ (Paths.nix, args, [ӭ (ә "NIX_CONFIG")], mock_set)
  flake_show ← nixDo (𝕵 mock_set) args
  x ∷ FlakePkgs ← eAsAesonError (FlakePkgs d ⊳ eitherDecodeStrict' (encodeUtf8 flake_show))
  prios ← readPriorities (priosFile d)
  return (updatePriorities prios x)

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
      go pps (PkgPriority (p,y)) = case p `Map.lookup` pps of
                                     𝕹 → return $ Map.insert p y pps
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
