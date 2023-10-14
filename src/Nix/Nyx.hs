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

import Prelude ( (*) )

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError )

-- base --------------------------------

import Control.Applicative ( optional )
import Data.Foldable       ( Foldable )
import Data.Function       ( flip )
import Data.Functor        ( Functor )
import Data.List           ( any, intersect, repeat, sort, transpose, zip,
                             zipWith )
import Data.List.NonEmpty  ( nonEmpty )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- data-textual ------------------------

import Data.Textual ( Textual(textual) )

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD, AbsF) )
import FPath.AbsDir           ( AbsDir )
import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Basename         ( basename )
import FPath.Dirname          ( dirname )
import FPath.Error.FPathError ( AsFPathError )
import FPath.Parseable        ( parse )
import FPath.PathComponent    ( PathComponent )
import FPath.RelDir           ( reldir )
import FPath.RelFile          ( relfile )
import FPath.ToDir            ( ToDir(toDir) )

-- lens --------------------------------

import Control.Lens.Each   ( each )
import Control.Lens.Fold   ( (^..) )
import Control.Lens.Getter ( view )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock(NoMock), HasDoMock(doMock) )
import MockIO.IOClass ( HasIOClass )

-- mockio-log --------------------------

import MockIO.Log ( MockIOClass, infoIO, warnIO )

-- mockio-plus -------------------------

import MockIO.Directory ( lsdir' )
import MockIO.Process   ( ꙩ )

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

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )

-- natural -----------------------------

import Natural ( length, replicate )

-- optparse-applicative ----------------

import Options.Applicative.Builder     ( command, flag', help, info, long,
                                         progDesc, short, strArgument,
                                         strOption, subparser )
import Options.Applicative.Help.Pretty ( empty, vcat )
import Options.Applicative.Types       ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

-- safe --------------------------------

import Safe ( maximumDef )

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

import Nix.Paths          qualified as Paths
import Nix.Types.AttrPath qualified as AttrPath

import Nix.Error            ( AsNixError, NixProgramError )
import Nix.Flake            ( FlakePkg, FlakePkgs, flakeShow, flakeShow', pkg,
                              pkgFindNames', ver, x86_64_, x86_64_pkgs )
import Nix.Profile          ( nixProfileAbsDir )
import Nix.Profile.Manifest ( attrPaths, readManifestDir )
import Nix.Types            ( Pkg(Pkg), ProfileDir )
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

------------------------------------------------------------

{- Given a list of lines, each being a list of columns; pad out the columns
   to provide an aligned display.

   The columns are padded out according to the input `pads` argument.  Widths
   are set according to the widest input column.  Columns for which no justify
   value is provided are left unmolested.
-}
data Justify = JustifyLeft | JustifyRight

-- provide fixed width args, and ignore args, and centrejustify args

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

------------------------------------------------------------

-- newtype PkgName = PkgName 𝕋
data Packages = AllPackages
              | SomePackages (NonEmpty Pkg)

data Mode = ModeListPkgs (𝕄 𝕋)
          | ModeListConfigs
          | ModeInstall Packages (𝕄 𝕋)

newtype Options = Options { mode :: Mode }

----------------------------------------

{-| cmdline options parser -}
parseOptions ∷ Parser Options
parseOptions =
  let
    config_option = strOption (ю [ short 'c', long "config"
                                 , help "select config to use" ])
    install_parser ∷ Parser Mode
    install_parser =
      ModeInstall ⊳ (  (SomePackages ⊳ parseNE (Pkg ⊳ strArgument (help "package")))
                     ∤ (flag' AllPackages (ю [ short 'a'
                                                  , help "all packages" ])))
                  ⊵ optional config_option

  in
    Options ⊳ subparser
    (ю [ command "list-packages"    (info (ModeListPkgs ⊳
                                           optional config_option)
                                     (progDesc "list packages"))
       , command "list-config-dirs" (info (pure ModeListConfigs)
                                     (progDesc "list config directories"))
       , command "install"          (info install_parser
                                     (progDesc "install one or more packages")
                                    )
       ])

------------------------------------------------------------

{-| top dir to look for config flakes -}
configTop ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            μ AbsDir
configTop = homePath [reldir|nix/|]

----------------------------------------

newtype ConfigName = ConfigName { unConfigName :: PathComponent }
  deriving (Printable)

instance Textual ConfigName where
  textual = ConfigName ⊳ textual

----------------------------------------

configDir ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            ConfigName → μ AbsDir
configDir p = (⫻ fromList [unConfigName p]) ⊳ configTop

----------------------------------------

{-| top dir to look for config flakes -}
configDefault ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
                μ AbsDir
configDefault = (⫻ [reldir|default/|]) ⊳ configTop

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

namePkgVers ∷ FlakePkgs → [(𝕋,𝕋,𝕋)]
namePkgVers pkgs =
  let
    pkgVer ∷ FlakePkg → (𝕋,𝕋)
    pkgVer fp = (toText $ fp ⊣ pkg, maybe "" toText $ fp ⊣ ver)

    pfx ∷ α → (β,γ) → (α,β,γ)
    pfx x (y,z) = (x,y,z)
  in
    Map.foldMapWithKey (\ p fp → [pfx (toText p) (pkgVer fp)]) $ pkgs ⊣ x86_64_

----------------------------------------

natNeg ∷ ℕ → ℕ → ℕ
natNeg x y = if x ≥ y then x - y else 0

(⊖) ∷ ℕ → ℕ → ℕ
(⊖) = natNeg

data NumSign = SignPlus | SignMinus

unNegate ∷ ℤ → (NumSign,ℕ)
unNegate n | n < 0     = (SignMinus, fromIntegral $ abs n)
           | otherwise = (SignPlus,  fromIntegral n)

----------------------------------------

{- | If f is a file type then if it is a dir on disc convert it else issue a
     warning and use the base dir; if f is a dir, use that. -}

configDirFromAbs ∷ (MonadIO μ, Printable ε,
                    AsFPathError ε, AsIOError ε, MonadError ε μ)⇒
                   𝕋 → μ AbsDir
configDirFromAbs f = do
  pResolve f ≫ \ case
    AbsD d → return d
    AbsF f' → isDir f' ≫ \ case
      𝕿 → return $ toDir f'
      𝕱 → if basename f' ≡ [relfile|flake.nix|]
          then return $ f' ⊣ dirname
          else parse @PathComponent f ≫ configDir ∘ ConfigName

----------------------------------------

nixDo ∷ ∀ ε δ φ μ . (MonadIO μ, Foldable φ, MonadReader δ μ, HasDoMock δ,
                     AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                     AsProcExitError ε, Printable ε,
                     MonadError ε μ, MonadLog (Log MockIOClass) μ) ⇒
        φ 𝕋 → μ ()
nixDo args = snd ⊳ ꙩ (Paths.nix, toList args)

----------------------------------------

mkTargets ∷ (Functor φ, Printable τ) ⇒ AbsDir → φ τ → φ 𝕋
mkTargets config_dir attr_paths = [fmt|%T#%T|] config_dir ⊳ attr_paths

----------------------------------------

info' ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η()
info' t = asks (view doMock) ≫ \ mock → infoIO mock t

warn' ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η()
warn' t = asks (view doMock) ≫ \ mock → warnIO mock t

msg ∷ ∀ τ δ φ η . (MonadIO η, Foldable φ, Printable τ,
                   HasDoMock δ, MonadReader δ η, MonadLog (Log MockIOClass) η) ⇒
      𝕋 → τ → φ AttrPath → η ()
msg verb object attr_paths = do
  let names = sort $ toText ∘ view AttrPath.pkg ⊳ toList attr_paths
  warn' $ [fmt|%t: %L|] verb names
  info' $ [fmt|%t: (%T) %L|] verb object attr_paths

----------------------------------------

nixBuild ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                      AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                      AsProcExitError ε, Printable ε, MonadError ε μ,
                      MonadLog (Log MockIOClass) μ) ⇒
           AbsDir → NonEmpty AttrPath → μ ()
nixBuild config_dir attr_paths = do
  msg "building" config_dir attr_paths
  let targets = mkTargets config_dir attr_paths
  nixDo $ [ "build", "--log-format", "bar-with-logs", "--no-link" ] ⊕
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
  nixDo $ ["profile", "remove", "--verbose", "--profile", toText profile] ⊕
          (toText ⊳ attr_paths)

----------------------------------------

nixProfileInstall ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                              AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                              AsProcExitError ε, Printable ε, MonadError ε μ,
                              MonadLog (Log MockIOClass) μ) ⇒
                   AbsDir → ProfileDir → NonEmpty AttrPath → μ ()
nixProfileInstall config_dir profile attr_paths = do
  msg "installing" ([fmtT|%T→%T|] config_dir profile) attr_paths
  let targets = mkTargets config_dir attr_paths
  nixDo $ [ "profile", "install", "--profile", toText profile ] ⊕
          (toList targets)

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
                 Printable ε, MonadError ε μ) ⇒
                (AbsDir → ProfileDir → NonEmpty AttrPath → μ α) → Maybe 𝕋
              → Packages → μ α
checkPackages f c pkgs = do
  config_dir     ← maybe configDefault configDirFromAbs c
  target_profile ← nixProfileAbsDir c

  flkPkgs ∷ FlakePkgs ← flakeShow' config_dir

  pkgs' ∷ NonEmpty Pkg ← case pkgs of
            SomePackages ps → return ps
            AllPackages →
              case nonEmpty $ x86_64_pkgs flkPkgs of
                𝕹    → throwUserError $ [fmtT|no packages found: %T|] config_dir
                𝕵 ps → return ps
  partitionMaybes ∘ toList ⊳ pkgFindNames' flkPkgs pkgs' ≫ \ case
    (missing:[],_) →
      throwUsage $ [fmtT|package not found: %T|] missing
    (missing@(_:_:_),_) →
      throwUsage $ [fmtT|packages not found: %L|] missing
    ([],pkgs'') → case  nonEmpty (snd ⊳ pkgs'') of
                    𝕵 attr_paths → f config_dir target_profile attr_paths
                    𝕹 →
                      throwUsage $ ("internal error: nonEmpty pkgs' means " ∷ 𝕋)
                                 ⊕ "this should never happen"

----------------------------------------

mainInstall ∷ ∀ ε δ μ .
              (MonadIO μ, AsProcExitError ε, AsCreateProcError ε,
               AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
               HasDoMock δ, MonadReader δ μ, MonadLog (Log MockIOClass) μ) ⇒
              AbsDir → ProfileDir → NonEmpty AttrPath → μ Word8

mainInstall config_dir target_profile attr_paths = do
  -- test build all the packages, before we make any destructive changes to
  -- the profile
  nixBuild config_dir attr_paths

  profile_manifest ← noMock $
    readManifestDir Informational target_profile ≫ either throwUserError return

  -- pre-remove anything found in the manifest; we're replacing/updating,
  -- rather than adding

  -- we do it this way because nix profile upgrade doesn't work with
  -- our flakes; e.g.,
  {- $ nix profile upgrade --profile /nix/var/nix/profiles/per-user/martyn/haskell /home/martyn/nix/haskell#packages.x86_64-linux.ghc
     warning: '/home/martyn/nix/haskell#packages.x86_64-linux.ghc' does not match any packages
  -}
  -- true even if we use, e.g.,
  -- git+file:///home/martyn/nix/haskell#packages.x86_64-linux.ghc
  -- cited by nix profile list

  -- nix profile install adds a new package without removing the older one

  let removals = intersect (attrPaths profile_manifest) (toList attr_paths)
  nixProfileRemove target_profile removals
  nixProfileInstall config_dir target_profile attr_paths
  return 0

----------------------------------------

{-| List all the packages from a given flake -}
mainListPkgs ∷ (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                AsAesonError ε, AsProcExitError ε, AsCreateProcError ε,
                AsFPathError ε, AsIOError ε, Printable ε,
                MonadError ε μ, MonadLog (Log MockIOClass) μ) ⇒
               𝕄 𝕋 → μ Word8
mainListPkgs f = do
  config_dir ← maybe configDefault configDirFromAbs f
  xs ∷ [(𝕋,𝕋,𝕋)] ← namePkgVers ⊳ flakeShow config_dir
  let xs' = tupleToList ⊳ xs
  forM_ (columnify [JustifyLeft, JustifyLeft, JustifyRight] xs')
        (say ∘ intercalate "\t")
  return 0

----------------------------------------

myMain ∷ (HasCallStack, AsNixError ε, AsIOError ε,AsFPathError ε,AsAesonError ε,
          AsCreateProcError ε, AsProcExitError ε, AsTextualParseError ε,
          AsUsageError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = flip runReaderT do_mock $
  case mode opts of
    ModeListPkgs f   → mainListPkgs f
    ModeInstall ps c → checkPackages mainInstall c ps
    ModeListConfigs  → allConfigDirs ≫ mapM_ say ⪼ return 0

{-| program main entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
-- show all configs (as option)
  let desc =
        vcat $ [ "manage nix configs for ~home installation", empty ]
  getArgs ≫ stdMain desc parseOptions (myMain @NixProgramError)

-- that's all, folks! ----------------------------------------------------------
