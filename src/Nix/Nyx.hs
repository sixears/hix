{-# LANGUAGE DeriveAnyClass        #-}
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

import Debug.Trace ( trace, traceShow )
import Prelude     ( Int, Integral, Num, (*) )

import Natural qualified

-- aeson -------------------------------

import Data.Aeson ( FromJSON(parseJSON), defaultOptions, eitherDecodeStrict',
                    fieldLabelModifier, genericParseJSON, withObject, (.:),
                    (.:?) )

-- aeson-plus --------------------------

import Data.Aeson.Error ( AesonError, AsAesonError(_AesonError),
                          throwAsAesonError )

-- base --------------------------------

import Data.List qualified

import Control.Applicative ( optional )
import Data.Foldable       ( Foldable )
import Data.Function       ( flip )
import Data.Functor        ( Functor )
import Data.List           ( any, filter, maximum, repeat, transpose, unzip3,
                             zip, zipWith )
import Data.Traversable    ( traverse )
import GHC.Generics        ( Generic )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- data-textual ------------------------

import Data.Textual ( Parsed(Malformed, Parsed), Textual(textual) )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD, AbsF) )
import FPath.AbsDir           ( AbsDir, absdir )
import FPath.AbsFile          ( AbsFile, absfile )
import FPath.AppendableFPath  ( AppendableFPath, AppendableFPathD,
                                AppendableFPathF, (⫻) )
import FPath.AsFilePath       ( AsFilePath, filepath )
import FPath.Basename         ( basename )
import FPath.Dir              ( Dir, DirAs )
import FPath.Dirname          ( dirname )
import FPath.DirType          ( DirType )
import FPath.Error.FPathError ( AsFPathError(_FPathError) )
import FPath.File             ( File )
import FPath.Parseable        ( parse, readM )
import FPath.PathComponent    ( PathComponent )
import FPath.RelDir           ( RelDir, reldir )
import FPath.RelFile          ( RelFile, relfile )
import FPath.ToDir            ( ToDir(toDir) )
import Text.Read              ( Read )

-- lens --------------------------------

import Control.Lens.Each   ( each )
import Control.Lens.Fold   ( (^..) )
import Control.Lens.Getter ( Getting, view )
import Control.Lens.Setter ( over )
import Control.Lens.Tuple  ( _1, _2, _3 )

-- log-plus ----------------------------

import Log ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock(NoMock) )
import MockIO.IOClass ( HasIOClass, IOClass(IORead, IOWrite), ioClass )

-- mockio-log --------------------------

import MockIO.Log ( HasDoMock, MockIOClass, doMock, logResult, mkIOL, mkIOLME,
                    mkIOLMER )

-- mockio-plus -------------------------

import MockIO.Directory             ( lsdir' )
import MockIO.File                  ( FExists(FExists), fexists )
import MockIO.FStat                 ( stat' )
import MockIO.Process               ( ꙫ )
import MockIO.Process.MLCmdSpec     ( MLCmdSpec, ToMLCmdSpec, mock_value )
import MockIO.Process.OutputDefault ( OutputDefault )

-- monaderror-io -----------------------

import MonadError.IO.Error ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                       ( say, warn )
import MonadIO.Base                  ( getArgs )
import MonadIO.Cwd                   ( getCwd )
import MonadIO.Error.CreateProcError ( AsCreateProcError(_CreateProcError) )
import MonadIO.Error.ProcExitError   ( AsProcExitError(_ProcExitError) )
import MonadIO.FPath                 ( pResolve, pResolveDir )
import MonadIO.FStat                 ( isDir )
import MonadIO.Process.ExitInfo      ( ExitInfo )
import MonadIO.Process.ExitStatus    ( ExitStatus, evOK )
import MonadIO.Process.MakeProc      ( MakeProc )
import MonadIO.Process.OutputHandles ( OutputHandles )
import MonadIO.Process.ToMaybeTexts  ( ToMaybeTexts )
import MonadIO.User                  ( getUserName', homePath )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad ( (⋙) )

-- mtl ---------------------------------

import Control.Monad.Except ( throwError )
import Control.Monad.Reader ( MonadReader, runReaderT )

-- natural -----------------------------

import Natural ( length, replicate )

-- optparse-applicative ----------------

import Options.Applicative.Builder     ( argument, auto, command, idm, info,
                                         metavar, progDesc, strArgument,
                                         subparser )
import Options.Applicative.Help.Pretty ( align, empty, fillSep, text, vcat,
                                         (<$$>) )
import Options.Applicative.Types       ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( readT, toDocTs, (⊞) )

-- parsers -----------------------------

import Text.Parser.Char ( string )

-- safe --------------------------------

import Safe ( maximumDef )

-- stdmain -----------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError(_UsageError), UsageFPathIOError,
                            UsageParseAesonFPPIOError, UsageParseFPProcIOError,
                            throwUsage )

-- text --------------------------------

import Data.Text          ( concat, intercalate, pack, unpack )
import Data.Text.Encoding ( encodeUtf8 )
-- textual-plus ------------------------

import TextualPlus qualified

import TextualPlus.Error.TextualParseError ( AsTextualParseError(_TextualParseError),
                                             TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Paths qualified as Paths

import Nix.Flake ( FlakePkg, FlakePkgs, flakeShow, forMX86_64Pkg,
                   forMX86_64Pkg_, forX86_64Pkg, pkg, ver, x86_64, x86_64_ )
import Nix.Types ( Arch, Pkg, Ver, pkgRE )

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

newtype ProfileName = ProfileName { unProfileName :: PathComponent }

userProfileName ∷ ∀ ε μ .
              (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
              μ ProfileName
userProfileName = ProfileName ⊳ (getUserName' ≫ parse)

userProfileNameRelDir ∷ ∀ ε μ .
                        (MonadIO μ,
                         AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
                        μ RelDir
userProfileNameRelDir = (fromList ∘ pure ∘ unProfileName) ⊳ userProfileName

------------------------------------------------------------

data Mode = ModeListPkgs (𝕄 𝕋)
          | ModeListConfigs

instance Printable Mode where

{-
instance Textual Mode where
  textual = string "list-packages"    ⋫ (ModeListPkgs ⊵ _)
          ∤ string "list-config-dirs" ⋫ pure ModeListConfigs
-}

-- XXX This should be a sub-command or similar

newtype Options = Options { mode :: Mode }

----------------------------------------

{-| cmdline options parser -}
parseOptions ∷ Parser Options
parseOptions = Options ⊳ subparser
  (ю [ command "list-packages"    (info (ModeListPkgs ⊳ optional (strArgument idm))
                                 (progDesc "list packages"))
     , command "list-config-dirs" (info (pure ModeListConfigs)
                                 (progDesc "list config directories"))
     ])

--  argument readT (metavar "MODE")

------------------------------------------------------------

{-| top dir to look for config flakes -}
configTop ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒ μ AbsDir
configTop = homePath [reldir|nix/|]

----------------------------------------

{-| top dir to look for config flakes -}
configDefault ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
                μ AbsDir
configDefault = (⫻ [reldir|default/|]) ⊳ configTop

----------------------------------------

{-| top dir where nix profiles are stored -}
profilesTop ∷ (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
              μ AbsDir
profilesTop =
  ([absdir|/nix/var/nix/profiles/per-user/|] ⫻) ⊳ userProfileNameRelDir

----------------------------------------

{-| append a profile name to a dir to find a profile dir -}
profileAppend ∷ AbsDir → ProfileName → AbsDir
profileAppend top = (top ⫻) ∘ fromList ∘ pure ∘ unProfileName

----------------------------------------

{-| find a profile dir from a profile name, assuming the use of `profilesTop` -}
profileDir ∷ (MonadIO μ,AsIOError ε,AsFPathError ε,Printable ε,MonadError ε μ)⇒
             ProfileName → μ AbsDir
profileDir = (profilesTop ⊲) ∘ flip profileAppend

----------------------------------------

{-| A variant of `lsdir'` that just returns the subdirectories.  For complex
    type issues that I do not grok; it only works for `AbsDir`. -}
subdirs ∷ ∀ ε ω μ .
          (MonadIO μ,
           AsFPathError ε,AsIOError ε,Printable ε,MonadError ε μ,HasCallStack,
           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
          Severity → AbsDir → DoMock → μ [AbsDir]
subdirs sv d k = fst ⊳⊳ snd ⊳ lsdir' @_ @AbsFile sv d k

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

{-| list of config flakes -}

allConfigs ∷ (MonadIO μ,
              HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ,
              AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
             μ [AbsFile]
allConfigs = do
  config_top  ← configTop
  let subdirs_ d = subdirs Informational d NoMock
      fexists_ f = (FExists ≡) ⊳ fexists Informational FExists f NoMock
  proto_flakes ← (⫻ [relfile|flake.nix|]) ⊳⊳ subdirs_ config_top
  (return proto_flakes) ≫ filterM fexists_

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

padT ∷ ℤ → 𝕋 → 𝕋
padT (unNegate → (SignMinus,n)) t = replicate @𝕋 (n ⊖ length t) ' ' ⊕ t
padT (unNegate → (SignPlus, n)) t = t ⊕ replicate @𝕋 (n ⊖ length t) ' '

{- | If f is a file type then if it is a dir on disc convert it else issue a
     warning and use the base dir; if f is a dir, use that. -}

flakeDirFromAbs ∷ (MonadIO μ,
                   AsFPathError ε, AsIOError ε, AsUsageError ε, MonadError ε μ)⇒
                  𝕋 → μ AbsDir
flakeDirFromAbs f = do
  pResolve f ≫ \ case
    AbsD d → return d
    AbsF f' → isDir f' ≫ \ case
      𝕿 → return $ toDir f'
      𝕱 → if basename f' ≡ [relfile|flake.nix|]
           then return $ f' ⊣ dirname
           else throwUsage $ [fmtT|cannot use '%T' as flake|] f

myMain ∷ (HasCallStack, AsUsageError ε, AsIOError ε, AsFPathError ε,
          AsCreateProcError ε, AsProcExitError ε, AsAesonError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = flip runReaderT do_mock $
  case mode opts of
    ModeListPkgs f -> do
      flake_dir ← case f of
        𝕹   → configDefault
        𝕵 f → flakeDirFromAbs f
      say flake_dir
      allConfigs ≫ mapM_ say

      xs ← flakeShow flake_dir ≫ either throwAsAesonError (return ∘ namePkgVers)

      let xs' = tupleToList ⊳ xs
      forM_ (columnify [JustifyLeft, JustifyLeft, JustifyRight] xs')
            (say ∘ intercalate "\t")
      return 0

{-| program main entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
-- ?add logging options
-- show all configs (as option)
  let progDesc =
        -- the values of list_packages, etc., will be wrapped as necessary
        let list_packages = fillSep [ toDocTs [ "list the available packages of a config"] ]
        in vcat [ "manage nix configs for ~home installation\n\nModes:"
                , empty <$$> text "list-packages" ⊞ align list_packages ]
  getArgs ≫ stdMain progDesc parseOptions (myMain @UsageParseAesonFPPIOError)

-- that's all, folks! ----------------------------------------------------------
