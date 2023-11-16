-- add version-based upgrading semantic
-- add handling of different architectures

-- add Handle for default profile (~/.nix-profile) (make it a required arg)
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

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError )

-- base --------------------------------

import Data.Foldable      ( Foldable, concat )
import Data.Function      ( flip )
import Data.List          ( intersect, sort, sortOn )
import Data.List.NonEmpty ( nonEmpty )
import Data.Tuple         ( swap, uncurry )

-- columnify ---------------------------

import Text.Columnify ( Justify(JustifyLeft, JustifyRight), columnify )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter ( view )
import Control.Lens.Tuple  ( _1 )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Notice) )

-- mockio ------------------------------

import MockIO        ( noMock )
import MockIO.DoMock ( DoMock, HasDoMock )

-- mockio-log --------------------------

import MockIO.Log             ( MockIOClass )
import MockIO.Log.MonadReader ( debug, info )

-- monaderror-io -----------------------

import MonadError.IO.Error ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                       ( say )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad ( (⮞) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, runReaderT )

-- optparse-applicative ----------------

import Options.Applicative.Help.Pretty ( empty, vcat )

-- stdmain -----------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsageT )

-- text --------------------------------

import Data.Text ( intercalate )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

-- tuple-plus --------------------------

import Data.TuplePlus ( tupleToList )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Error            ( AsNixError, NixProgramError )
import Nix.Flake            ( FlakePkgs, flakeShowNM, namePkgVersPrioSrcArch,
                              pkgFindNames', x86_64_pkgs )
import Nix.NixExe           ( nixBuild, nixProfileInstall, nixProfileRemove )
import Nix.Nyx.Options      ( Configs(AllConfigs, SomeConfigs),
                              Mode(ModeInstall, ModeListConfigNames, ModeListConfigs, ModeListPkgs),
                              Options, Packages(AllPackages, SomePackages),
                              mode, parseOptions, remote_state )
import Nix.Profile          ( nixProfileAbsDir )
import Nix.Profile.Manifest ( attrPaths, readManifestDir )
import Nix.Types            ( Pkg, Priority, ProfileDir, RemoteState )
import Nix.Types.AttrPath   ( AttrPath )
import Nix.Types.ConfigDir  ( ConfigDir, allConfigDirs, allConfigNames,
                              configDirFromAbs )
import Nix.Types.ConfigName ( ConfigName(unConfigName), configDefault )

--------------------------------------------------------------------------------

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
            𝕹    → throwUsageT $ [fmt|no packages found: %T|] config_dir
            𝕵 ps → return ps
    partitionMaybes ∘ toList ⊳ pkgFindNames' flkPkgs pkgs' ≫ \ case
      (missing:[],_) →
        throwUsageT $ [fmt|package not found in %T: %T|] c missing
      (missing@(_:_:_),_) →
        throwUsageT $ [fmt|packages not found in %T: %L|] c missing
      ([],pkgs'' ∷ [(Pkg,(AttrPath, (𝕄 Priority)))]) →
        case nonEmpty (snd ⊳ pkgs'') of
          𝕵 attr_path_prios → do return (config_dir, target_profile,
                                         multiMap $ swap ⊳ attr_path_prios)
          𝕹 →
            throwUsageT $ intercalate " " [ "internal error: nonEmpty pkgs'"
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
  {- nix profile upgrade --profile \
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
