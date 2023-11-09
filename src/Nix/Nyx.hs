-- add Handle for default profile (~/.nix-profile)
-- add nix-install, nix-search equivs

{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-| nyx program to manage local configurations -}
module Nix.Nyx
  ( main
  ) where

import Base1T

import Prelude ( error )

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Data.Foldable      ( Foldable, concat )
import Data.Function      ( flip )
import Data.Functor       ( Functor )
import Data.List          ( any, intersect, sort, sortOn )
import Data.List.NonEmpty ( nonEmpty )
import Data.Tuple         ( swap, uncurry )

-- columnify ---------------------------

import Text.Columnify ( Justify(JustifyLeft, JustifyRight), columnify )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD, AbsF) )
import FPath.AbsDir           ( AbsDir )
import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Basename         ( basename )
import FPath.Dirname          ( dirname )
import FPath.Error.FPathError ( AsFPathError )
import FPath.Parseable        ( Parseable(parse) )
import FPath.PathComponent    ( PathComponent, pc )
import FPath.RelDir           ( reldir )
import FPath.RelFile          ( relfile )
import FPath.ToDir            ( ToDir(toDir) )

-- lens --------------------------------

import Control.Lens.Getter ( view )
import Control.Lens.Tuple  ( _1 )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational, Notice) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock(NoMock), HasDoMock(doMock) )
import MockIO.IOClass ( HasIOClass )

-- mockio-log --------------------------

import MockIO.Log ( MockIOClass, debugIO, infoIO, noticeIO, warnIO )

-- mockio-plus -------------------------

import MockIO.Directory ( lsdir' )

-- monaderror-io -----------------------

import MonadError.IO.Error ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                       ( say )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.FPath                 ( pResolve )
import MonadIO.FStat                 ( isDir )
import MonadIO.User                  ( homePath )

-- mono-traversable --------------------

import Data.MonoTraversable ( otoList )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )

-- optparse-applicative ----------------

import Options.Applicative.Help.Pretty ( empty, vcat )

-- safe --------------------------------

import Safe ( lastMay )

-- stdmain -----------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsage )

-- text --------------------------------

import Data.Text ( intercalate )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix ( nixDo )

import Nix.Types.AttrPath qualified as AttrPath

import Nix.Error            ( AsNixError, NixProgramError )
import Nix.Flake            ( FlakePkg, FlakePkgs, archMap, flakeShowNM,
                              location, pkg, pkgFindNames', priority, ver,
                              x86_64_pkgs )
import Nix.Nyx.Options      ( Configs(AllConfigs, SomeConfigs),
                              Mode(ModeInstall, ModeListConfigNames, ModeListConfigs, ModeListPkgs),
                              Options, Packages(AllPackages, SomePackages),
                              mode, parseOptions, remote_state )
import Nix.Profile          ( nixProfileAbsDir )
import Nix.Profile.Manifest ( attrPaths, readManifestDir )
import Nix.Types            ( Arch, ConfigDir(ConfigDir, unConfigDir),
                              ConfigName(ConfigName, unConfigName), Pkg,
                              Priority(unPriority), ProfileDir, RemoteState,
                              ToBriefText(toT) )
import Nix.Types.AttrPath   ( AttrPath )

--------------------------------------------------------------------------------

class HomogenousTuple α where
  type family TupleItem α
  tupleToList ∷ α → [TupleItem α]

instance HomogenousTuple (α,α) where
  type instance TupleItem (α,α) = α
  tupleToList (a0,a1) = [a0,a1]

instance HomogenousTuple (α,α,α) where
  type instance TupleItem (α,α,α) = α
  tupleToList (a0,a1,a2) = [a0,a1,a2]

instance HomogenousTuple (α,α,α,α) where
  type instance TupleItem (α,α,α,α) = α
  tupleToList (a0,a1,a2,a3) = [a0,a1,a2,a3]

instance HomogenousTuple (α,α,α,α,α) where
  type instance TupleItem (α,α,α,α,α) = α
  tupleToList (a0,a1,a2,a3,a4) = [a0,a1,a2,a3,a4]

instance HomogenousTuple (α,α,α,α,α,α) where
  type instance TupleItem (α,α,α,α,α,α) = α
  tupleToList (a0,a1,a2,a3,a4,a5) = [a0,a1,a2,a3,a4,a5]

------------------------------------------------------------

class TuplePrepend α β γ where
  type family TuplePrepended α β
  tuplePrepend ∷ α → β → γ
  (⨤) ∷ α → β → γ
  (⨤) = tuplePrepend

instance ∀ α β γ . TuplePrepend α (β,γ) (α,β,γ) where
  type instance TuplePrepended α (β,γ) = (α,β,γ)
  tuplePrepend α (β,γ) = (α,β,γ)

instance ∀ α β γ δ . TuplePrepend α (β,γ,δ) (α,β,γ,δ) where
  type instance TuplePrepended α (β,γ,δ) = (α,β,γ,δ)
  tuplePrepend α (β,γ,δ) = (α,β,γ,δ)

------------------------------------------------------------

class TupleAppend α β γ where
  type family TupleAppended α β
  tupleAppend ∷ α → β → γ
  (⨦) ∷ α → β → γ
  (⨦) = tupleAppend

instance ∀ α β γ . TupleAppend (α,β) γ (α,β,γ) where
  type instance TupleAppended (α,β) γ = (α,β,γ)
  tupleAppend (α,β) γ = (α,β,γ)

instance ∀ α β γ δ . TupleAppend (α,β,γ) δ (α,β,γ,δ) where
  type instance TupleAppended (α,β,γ) δ = (α,β,γ,δ)
  tupleAppend (α,β,γ) δ = (α,β,γ,δ)

instance ∀ α β γ δ κ . TupleAppend (α,β,γ,δ) κ (α,β,γ,δ,κ) where
  type instance TupleAppended (α,β,γ,δ) κ = (α,β,γ,δ,κ)
  tupleAppend (α,β,γ,δ) κ = (α,β,γ,δ,κ)

instance ∀ α β γ δ κ ι . TupleAppend (α,β,γ,δ,κ) ι (α,β,γ,δ,κ,ι) where
  type instance TupleAppended (α,β,γ,δ,κ) ι = (α,β,γ,δ,κ,ι)
  tupleAppend (α,β,γ,δ,κ) ι = (α,β,γ,δ,κ,ι)

------------------------------------------------------------

(⮞) ∷ (Monad η, Traversable ψ) ⇒ (α → η β) → ψ α → η (ψ β)
(⮞) = mapM

(⮚) ∷ (Monad η, Foldable φ) ⇒ (α → η ()) → φ α → η ()
(⮚) = mapM_

(⮜) ∷ (Monad η, Traversable ψ) ⇒ ψ α → (α → η β) → η (ψ β)
(⮜) = forM

(⮘) ∷ (Monad η, Foldable φ) ⇒ φ α → (α → η ()) → η ()
(⮘) = forM_

------------------------------------------------------------

debug ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
debug t = asks (view doMock) ≫ \ mock → debugIO mock t

info ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
info t = asks (view doMock) ≫ \ mock → infoIO mock t

notice ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
notice t = asks (view doMock) ≫ \ mock → noticeIO mock t

warn ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
warn t = asks (view doMock) ≫ \ mock → warnIO mock t

----------------------------------------

{-| A variant of `lsdir'` that just returns the subdirectories.  For complex
    type issues that I do not grok; it only works for `AbsDir`. -}
subdirs ∷ ∀ ε ω μ .
          (MonadIO μ,
           AsFPathError ε,AsIOError ε,Printable ε,MonadError ε μ,HasCallStack,
           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
          Severity → AbsDir → DoMock → μ [AbsDir]
subdirs sv d k = fst ⊳⊳ snd ⊳ lsdir' @_ @AbsFile sv d k

----------------------------------------

{- Given a list of lines, each being a list of columns; pad out the columns
   to provide an aligned display.

   The columns are padded out according to the input `pads` argument.  Widths
   are set according to the widest input column.  Columns for which no justify
   value is provided are left unmolested.
-}
-- data Justify = JustifyLeft | JustifyRight

-- provide fixed width args, and ignore args, and centrejustify args

{-
columnify ∷ [Justify] → [[𝕋]] → [[𝕋]]
columnify pads zs =
  let pad_t ∷ ℤ → 𝕋 → 𝕋
      pad_t (unNegate → (SignMinus,n)) t = replicate @𝕋 (n ⊖ length t) ' ' ⊕ t
      pad_t (unNegate → (SignPlus, n)) t = t ⊕ replicate @𝕋 (n ⊖ length t) ' '

      col_widths = transpose zs & each ⊧ (\ ys → maximumDef 0 $ length ⊳ ys)
      xx JustifyLeft  = 1
      xx JustifyRight = (-1)
      col_widths' = (\(x,y) → fromIntegral y * xx x) ⊳ zip pads col_widths
  in
    (^.. each) ∘ zipWith pad_t (col_widths' ⊕ repeat 0) ⊳ zs
-}

----------------------------------------

throwUsage' ∷ ∀ ε ω η . (AsUsageError ε, MonadError ε η) ⇒ 𝕋 → η ω
throwUsage' = throwUsage

------------------------------------------------------------

{-| top dir to look for config flakes -}
configTop ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            μ AbsDir
configTop = homePath [reldir|nix/|]

----------------------------------------

configDir ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            ConfigName → μ ConfigDir
configDir p = ConfigDir ⊳ ((⫻ fromList [unConfigName p]) ⊳ configTop)

----------------------------------------

{-| top dir to look for config flakes -}
configDefault ∷ ConfigName
configDefault = ConfigName [pc|default|]

----------------------------------------

{-| list of config directories; that is, dirs in `configTop` that contain a
    @flake.nix@ -}
allConfigDirs ∷ (MonadIO μ,
              HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ,
              AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
             μ [AbsDir]
allConfigDirs = do
  config_top  ← configTop
  let has_flake ∷ (MonadIO μ,
                   AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,
                   HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                  AbsDir → μ 𝔹
      has_flake d  = do
        (fs,_) ← lsdir' @_ @AbsFile Informational d NoMock
        return $ any (\ (fn, _) → [relfile|flake.nix|] ≡ basename fn) fs
  subdirs Informational config_top NoMock ≫ filterM has_flake


----------------------------------------

allConfigNames ∷ (MonadIO μ,
                  HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ,
                  AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
                 μ [ConfigName]
allConfigNames = basePC ⊳⊳ allConfigDirs
  where basePC ∷ AbsDir → ConfigName
        basePC dir = case lastMay ∘ otoList $ basename dir of
                        𝕹   → error $ [fmt|could not find ConfigName of %T|] dir
                        𝕵 p → ConfigName p

----------------------------------------

namePkgVersPrioSrcArch ∷ FlakePkgs → [(𝕋,𝕋,𝕋,𝕋,𝕋,𝕋)]
namePkgVersPrioSrcArch pkgs =
  let
    pkgVer ∷ FlakePkg → (𝕋,𝕋,𝕋)
    pkgVer fp = (toText $ fp ⊣ pkg, maybe "" toText $ fp ⊣ ver,
                 maybe "" toText $ fp ⊣ priority)

    go ∷ Pkg → FlakePkg → [(𝕋,𝕋,𝕋,𝕋,𝕋)]
    go p fp = [(toText p ⨤ (pkgVer fp) ∷ (𝕋,𝕋,𝕋,𝕋)) ⨦ toText (pkgs ⊣ location)]
    go' ∷ Arch → Map.Map Pkg FlakePkg → [(𝕋,𝕋,𝕋,𝕋,𝕋,𝕋)]
    go' arch fpmap = (⨦ (toText arch)) ⊳ Map.foldMapWithKey go fpmap
  in
    Map.foldMapWithKey go' (pkgs ⊣ archMap)

----------------------------------------

{- | Given the name of a config (e.g., "haskell"); find the flake directory for
     that config (e.g., ~/nix/haskell/).

     If f is a file type then if it is a dir on disc convert it else issue a
     warning and use the base dir; if f is a dir, use that.
-}

configDirFromAbs ∷ (MonadIO μ, Printable ε,
                    AsFPathError ε, AsIOError ε, MonadError ε μ)⇒
                   ConfigName → μ ConfigDir
configDirFromAbs f = do
  pResolve f ≫ \ case
    AbsD d → return $ ConfigDir d
    AbsF f' → isDir f' ≫ \ case
      𝕿 → return ∘ ConfigDir $ toDir f'
      𝕱 → if basename f' ≡ [relfile|flake.nix|]
          then return ∘ ConfigDir $ f' ⊣ dirname
          else parse @PathComponent f ≫ configDir ∘ ConfigName

----------------------------------------

mkTargets ∷ (Functor φ, Printable τ) ⇒ ConfigDir → φ τ → φ 𝕋
mkTargets config_dir attr_paths =
  [fmt|%T#%T|] (unConfigDir config_dir) ⊳ attr_paths

----------------------------------------

msg ∷ ∀ τ δ φ η . (MonadIO η, Foldable φ, Printable τ, ToBriefText τ,
                   HasDoMock δ, MonadReader δ η, MonadLog (Log MockIOClass) η) ⇒
      𝕋 → τ → φ AttrPath → η ()
msg verb object attr_paths = do
  let names = sort $ toText ∘ view AttrPath.pkg ⊳ toList attr_paths
  warn $ [fmt|%t (%t): %L|] verb (toT object) names
  notice $ [fmt|%t: (%T) %L|] verb object attr_paths

----------------------------------------

nixBuild ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                      AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                      AsProcExitError ε, Printable ε, MonadError ε μ,
                      MonadLog (Log MockIOClass) μ) ⇒
           ConfigDir → NonEmpty AttrPath → μ ()
nixBuild config_dir attr_paths = do
  msg "building" config_dir attr_paths
  let targets = mkTargets config_dir attr_paths
  nixDo 𝕹 $ [ "build", "--log-format", "bar-with-logs", "--no-link" ] ⊕
             (toList targets)

----------------------------------------

nixProfileRemove ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                              AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                              AsProcExitError ε, Printable ε, MonadError ε μ,
                              MonadLog (Log MockIOClass) μ) ⇒
                   ProfileDir → [AttrPath] → μ ()
nixProfileRemove _ [] = return ()
nixProfileRemove profile attr_paths = do
  msg "removing" profile attr_paths
  nixDo 𝕹 $ ["profile", "remove", "--verbose", "--profile", toText profile] ⊕
             (toText ⊳ attr_paths)

----------------------------------------

nixProfileInstall ∷ ∀ ε δ μ .
                    (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                     AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                     AsProcExitError ε, Printable ε, MonadError ε μ,
                     MonadLog (Log MockIOClass) μ) ⇒
                    ConfigDir → ProfileDir → 𝕄 Priority→NonEmpty AttrPath→μ ()
nixProfileInstall config_dir profile prio_m attr_paths = do
  let verb = maybe "" [fmt| «prio %T»|] prio_m
  msg ("installing" ◇ verb) (config_dir, profile) (NonEmpty.sort attr_paths)
  let targets = mkTargets config_dir attr_paths
  let extra_args = maybe [] (\ p → ["--priority", [fmt|%d|] (unPriority p)])
                         prio_m
  nixDo 𝕹 $ ю [ [ "profile", "install", "--profile", toText profile ]
              , extra_args, toList targets ]

----------------------------------------

noMock ∷ ∀ η α . ReaderT DoMock η α → η α
noMock = flip runReaderT NoMock

----------------------------------------

partitionMaybes ∷ [(α, 𝕄 β)] → ([α], [(α,β)])
partitionMaybes = go ([],[])
  where go (naes,yaes) []             = (naes, yaes)
        go (naes,yaes) ((a,𝕹) : xs)   = go (a:naes, yaes) xs
        go (naes,yaes) ((a,𝕵 b) : xs) = go (naes, (a,b) : yaes) xs

----------------------------------------

checkPackages ∷ ∀ ε α μ .
                (MonadIO μ, MonadLog (Log MockIOClass) μ,
                 AsUsageError ε, AsIOError ε, AsFPathError ε, AsAesonError ε,
                 AsCreateProcError ε, AsProcExitError ε, AsNixError ε,
                 AsTextualParseError ε, Printable ε, MonadError ε μ) ⇒
                (ConfigDir → ProfileDir
                           → Map.Map (𝕄 Priority) (NonEmpty AttrPath) → μ ())
              → (ConfigDir → ProfileDir
                           → Map.Map (𝕄 Priority) (NonEmpty AttrPath) → μ α)
              → RemoteState → [ConfigName] → Packages → μ Word8
checkPackages check go r [] pkgs = checkPackages check go r [configDefault] pkgs
checkPackages check go r cs pkgs = do
  -- targets ∷ [(ConfigDir,ProfileDir,NonEmpty AttrPath)]
  targets ← collectPackages r cs pkgs
  -- we split into 'check' and 'go' so that we can do pre-emptively make all the
  -- necessary checks before making any destructive changes
  forM_ targets (\ (cd,pd,aps) → check cd pd aps)
  forM_ targets (\ (cd,pd,aps) → go cd pd aps)
  return 0

----------------------------------------

multiMap ∷ (Foldable ψ, Ord κ) ⇒ ψ (κ,ν) → Map.Map κ (NonEmpty ν)
multiMap = Map.fromListWith (◇) ∘ fmap (second pure) ∘ toList

----------------------------------------

{-| Find what packages to install.

    Given some config names (desktop, haskell, scripts, etc.) and some package
    names (emacs, audacious, etc.); return a load of tuples of `ConfigDir`
    (source to build from), `ProfileDir` (profile to install to) and `AttrPath`s
    (things to install).

    If a number of packages are requested, they must exist in each specified
    `ConfigDir`.  The use of multiple configs probably makes sense only with
    `AllPackages`.
-}
collectPackages ∷ ∀ ε ψ μ .
                  (MonadIO μ, Traversable ψ, MonadLog (Log MockIOClass) μ,
                   AsUsageError ε, AsIOError ε, AsFPathError ε, AsAesonError ε,
                   AsCreateProcError ε, AsProcExitError ε, AsNixError ε,
                   AsTextualParseError ε, Printable ε, MonadError ε μ) ⇒
                  RemoteState → ψ ConfigName → Packages
                → μ (ψ (ConfigDir, ProfileDir,
                        Map.Map (𝕄 Priority) (NonEmpty AttrPath)))

collectPackages r cs pkgs =
  forM cs (\ c → do
    config_dir     ← configDirFromAbs c
    target_profile ← nixProfileAbsDir (toText $ unConfigName c)

    flkPkgs ∷ FlakePkgs ← flakeShowNM r config_dir

    pkgs' ∷ NonEmpty Pkg ←
      case pkgs of
        SomePackages ps → return ps
        AllPackages →
          case nonEmpty $ x86_64_pkgs flkPkgs of
            𝕹    → throwUsage' $ [fmt|no packages found: %T|] config_dir
            𝕵 ps → return ps
    partitionMaybes ∘ toList ⊳ pkgFindNames' flkPkgs pkgs' ≫ \ case
      (missing:[],_) →
        throwUsage $ [fmtT|package not found in %T: %T|] c missing
      (missing@(_:_:_),_) →
        throwUsage $ [fmtT|packages not found in %T: %L|] c missing
      ([],pkgs'' ∷ [(Pkg,(AttrPath, (𝕄 Priority)))]) →
        case nonEmpty (snd ⊳ pkgs'') of
          𝕵 attr_path_prios → do return (config_dir, target_profile,
                                         multiMap $ swap ⊳ attr_path_prios)
          𝕹 →
            throwUsage' $ intercalate " " [ "internal error: nonEmpty pkgs'"
                                          , "means this should never happen"])


----------------------------------------

installFromOneConfig ∷
  ∀ ε δ μ . (MonadIO μ, AsProcExitError ε, AsCreateProcError ε,
             AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
             HasDoMock δ, MonadReader δ μ, MonadLog (Log MockIOClass) μ) ⇒
            ConfigDir → ProfileDir → 𝕄 Priority → NonEmpty AttrPath → μ ()

installFromOneConfig config_dir target_profile prio_m attr_paths = do
  profile_manifest ← noMock $
    readManifestDir Notice target_profile ≫ either throwUserError return

  -- pre-remove anything found in the manifest; we're replacing/updating,
  -- rather than adding

  -- we do it this way because nix profile upgrade doesn't work with
  -- our flakes; e.g.,
  {- $ nix profile upgrade --profile \
             /nix/var/nix/profiles/per-user/martyn/haskell \
             /home/martyn/nix/haskell#packages.x86_64-linux.ghc
     warning: '/home/martyn/nix/haskell#packages.x86_64-linux.ghc' \
             does not match any packages
  -}
  -- true even if we use, e.g.,
  -- git+file:///home/martyn/nix/haskell#packages.x86_64-linux.ghc
  -- cited by nix profile list

  -- nix profile install adds a new package without removing the older one

  debug $ [fmt|manifest: %T|] profile_manifest
  info $ [fmt|manifest paths: %L|] (attrPaths profile_manifest)
  info $ [fmt|attr_path_prios: %L|] (toList attr_paths)
  let removals = intersect (attrPaths profile_manifest) (toList attr_paths)
  nixProfileRemove target_profile removals
  nixProfileInstall config_dir target_profile prio_m attr_paths
  return ()

----------------------------------------

configFlakePkgs ∷ ∀ ε μ .
                  (MonadIO μ, MonadLog (Log MockIOClass) μ,
                   AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                   AsProcExitError ε, AsAesonError ε, AsTextualParseError ε,
                  Printable ε, MonadError ε μ) ⇒
     RemoteState → [ConfigDir] → μ [FlakePkgs]
configFlakePkgs r config_dirs = (flakeShowNM r ⮞ config_dirs)

{-| Given a list of `ConfigDir`, generate a list rows, each representing
    a package from those config dirs.  Each row is:
    name (installation name), pkg (nix package name), pkg version,
    installation priority (if any), src (config dir in which the name/pkg is
    defined), arch (host architecture).
-}
configFlakeTxts ∷ ∀ ε μ .
                  (MonadIO μ, MonadLog (Log MockIOClass) μ,
                   AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                   AsProcExitError ε, AsAesonError ε, AsTextualParseError ε,
                  Printable ε, MonadError ε μ) ⇒
     RemoteState → [ConfigDir] → μ [(𝕋,𝕋,𝕋,𝕋,𝕋,𝕋)]
configFlakeTxts r config_dirs =
  ю ⊳ (namePkgVersPrioSrcArch ⊳⊳ configFlakePkgs r config_dirs)

{-| List all the packages from a given flake -}
mainListPkgs ∷ (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                AsAesonError ε, AsProcExitError ε, AsCreateProcError ε,
                AsFPathError ε, AsIOError ε, AsTextualParseError ε,
                Printable ε, MonadError ε μ, MonadLog (Log MockIOClass) μ) ⇒
               RemoteState → Configs → μ Word8
mainListPkgs r AllConfigs = allConfigNames ≫ mainListPkgs r ∘ SomeConfigs
mainListPkgs r (SomeConfigs []) = mainListPkgs r (SomeConfigs [configDefault])
mainListPkgs r (SomeConfigs cs) = do
  config_dirs ∷ [ConfigDir] ← mapM configDirFromAbs cs
  xs ← sortOn (view _1) ⊳ configFlakeTxts r config_dirs

  let xs' = tupleToList ⊳ xs
  forM_ (columnify [ JustifyLeft, JustifyLeft, JustifyRight,
                     JustifyLeft, JustifyLeft, JustifyLeft ] xs')
                (say ∘ intercalate "\t")
  return 0

----------------------------------------

myMain ∷ (HasCallStack,
          AsNixError ε, AsIOError ε, AsFPathError ε, AsAesonError ε,
          AsCreateProcError ε, AsProcExitError ε, AsTextualParseError ε,
          AsUsageError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = flip runReaderT do_mock $
  let
    r = opts ⊣ remote_state
  in
    case opts ⊣ mode of
      ModeListConfigs     → allConfigDirs ≫ mapM_ say ⪼ return 0
      ModeListConfigNames → allConfigNames ≫ mapM_ say ∘ sort ⪼ return 0
      ModeListPkgs cs     → mainListPkgs r cs
      ModeInstall cs ps   →
        let installFromOneConfigs cd pd =
              mapM_ (uncurry $ installFromOneConfig cd pd) ∘ Map.toList
            concat' ∷ [NonEmpty α] → NonEmpty α
            concat' xs = fromList $ concat (toList ⊳ xs)
        in  checkPackages (\ cd _ aps → nixBuild cd (concat' $ Map.elems aps))
                          installFromOneConfigs r cs ps

{-| program main entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
  let desc = vcat $ [ "manage nix configs for ~home installation", empty ]
  getArgs ≫ stdMain desc parseOptions (myMain @NixProgramError)

-- that's all, folks! ----------------------------------------------------------
