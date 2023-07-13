{-# LANGUAGE DeriveAnyClass #-}

{-| nyx program to manage local configurations -}
module Nix.Nyx
  ( main )
where

import Base1T

import Debug.Trace  ( trace, traceShow )
import Prelude  ( Int, Integral )

import qualified  Natural

-- aeson -------------------------------

import Data.Aeson  ( FromJSON( parseJSON )
                   , (.:), (.:?)
                   , defaultOptions, eitherDecodeStrict', fieldLabelModifier
                   , genericParseJSON, withObject
                   )

-- base --------------------------------

import Data.Foldable  ( Foldable )
import Data.Function  ( flip )
import Data.Functor   ( Functor )
import Data.List      ( any, filter, maximum )
import GHC.Generics   ( Generic )

-- containers --------------------------

import qualified Data.Map.Strict  as  Map

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir, absdir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( AppendableFPath, AppendableFPathD
                               , AppendableFPathF, (⫻) )
import FPath.AsFilePath        ( filepath )
import FPath.Basename          ( basename )
import FPath.Dir               ( DirAs )
import FPath.DirType           ( DirType )
import FPath.Error.FPathError  ( AsFPathError( _FPathError ) )
import FPath.Parseable         ( parse )
import FPath.PathComponent     ( PathComponent )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )
import FPath.ToDir             ( ToDir )
import Text.Read               ( Read )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Tuple   ( _1, _2 )

-- log-plus ----------------------------

import Log  ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Informational ) )

-- mockio ------------------------------

import MockIO.DoMock   ( DoMock( NoMock ) )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ), ioClass )

-- mockio-log --------------------------

import MockIO.Log  ( HasDoMock, MockIOClass
                   , doMock, logResult, mkIOL, mkIOLME, mkIOLMER )

-- mockio-plus -------------------------

import MockIO.Directory          ( lsdir' )
import MockIO.File               ( FExists( FExists ), fexists )
import MockIO.Process            ( ꙫ )
import MockIO.Process.MLCmdSpec  ( MLCmdSpec, mock_value )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                        ( say )
import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import MonadIO.Error.ProcExitError    ( AsProcExitError( _ProcExitError ) )
import MonadIO.Process.ExitInfo       ( ExitInfo )
import MonadIO.Process.ExitStatus     ( evOK )
import MonadIO.User                   ( homePath, getUserName' )

-- mtl ---------------------------------

import Control.Monad.Except  ( throwError )
import Control.Monad.Reader  ( runReaderT )

-- natural -----------------------------

import Natural  ( length, replicate )

-- optparse-applicative ----------------

import Options.Applicative.Builder      ( argument, auto, metavar )
import Options.Applicative.Help.Pretty  ( (<$$>)
                                        , align, empty, fillSep, text, vcat )
import Options.Applicative.Types        ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( (⊞), readT, toDocTs )

-- parsers -----------------------------

import Text.Parser.Char  ( string )

-- stdmain -----------------------------

import StdMain             ( stdMain )
import StdMain.UsageError  ( AsUsageError( _UsageError ), UsageFPathIOError
                           , UsageParseFPProcIOError
                           , UsageParseAesonFPPIOError
                           )

-- text --------------------------------

import Data.Text           ( concat, intercalate, pack, unpack )
import Data.Text.Encoding  ( encodeUtf8 )

-- textual-plus ------------------------

import qualified TextualPlus

import TextualPlus.Error.TextualParseError
               ( AsTextualParseError( _TextualParseError ), TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Nix.Paths  as  Paths

import Data.Aeson.Error  ( AesonError, AsAesonError( _AesonError )
                         , throwAsAesonError )

import Nix.Flake  ( FlakePkgs, forX86_64Pkg, forMX86_64Pkg, forMX86_64Pkg_, pkg
                  , ver, x86_64, x86_64_ )
import Nix.Types  ( Arch, Pkg, Ver, pkgRE )

--------------------------------------------------------------------------------

------------------------------------------------------------

newtype ProfileName = ProfileName { unProfileName ∷ PathComponent }

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

data Mode = ModeShowConfigs

instance Printable Mode where

instance Textual Mode where
  textual = string "show-configs" ⋫ pure ModeShowConfigs

data Options = Options { mode ∷ Mode }

----------------------------------------

{-| cmdline options parser -}
parseOptions ∷ Parser Options
parseOptions = Options ⊳ argument readT (metavar "MODE")

------------------------------------------------------------

{-| top dir to look for config flakes -}
configTop ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒ μ AbsDir
configTop = homePath [reldir|nix/|]

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
  config_dirs ←
    let has_flake ∷ (MonadIO μ,
                     AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,
                     HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                    AbsDir → μ 𝔹
        has_flake d  = do
          (fs,_) ← lsdir' @_ @AbsFile Informational d NoMock
          return $ any (\ (fn, _) → [relfile|flake.nix|] ≡ basename fn) fs
    in  subdirs Informational config_top NoMock ≫ filterM has_flake
  return $ config_dirs

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
  flakes ← (return proto_flakes) ≫ filterM fexists_
  return $ flakes

flakeShowTestInput =
  concat [ "{ \"packages\": {"
         , "    \"x86_64-linux\": {"

         , "      \"binutils\": {"
         , "        \"description\": \"Tools for manipulating binaries\","
         , "        \"name\": \"binutils-wrapper-2.38\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"get-iplayer-config\": {"
         , "        \"name\": \"get-iplayer-config\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"graph-easy\": {"
         , "        \"description\": \"Render/convert graphs\","
         , "        \"name\": \"perl5.34.1-Graph-Easy-0.76\","
         , "        \"type\": \"derivation\""
         , "      }"

         , "    }"
         , "  }"
         , "}"
         ]

-- nix flake show #flake
nix_flake_show flake =
  ꙫ (Paths.nix, ["flake", "show", "--json", pack $ flake ⫥ filepath],
     ((\ f → f & mock_value ⊢ (evOK,flakeShowTestInput)) ∷ MLCmdSpec 𝕋 → MLCmdSpec 𝕋)
    )

myMain ∷ ∀ ε . (HasCallStack,
                AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε, AsAesonError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock _ = flip runReaderT do_mock $ do
  allConfigs ≫ mapM_ say
  (_,(stdout∷𝕋)) ← nix_flake_show [absdir|/home/martyn/nix/default/|]
-- N-COLUMN DISPLAY
  xs' ← case eitherDecodeStrict' @FlakePkgs (encodeUtf8 stdout) of
          𝕽 pkgs → return ∘ Map.toList $ Map.map (\ fp → (fp ⊣ pkg, fp ⊣ ver)) $ pkgs ⊣ x86_64_
          𝕷 e    → throwAsAesonError e
  let max_l ∷ (Foldable ψ, Functor ψ, Printable α) ⇒ (β → α) → ψ β → ℕ
      max_l f = maximum ∘ fmap  (length ∘ toText ∘ f)
      fromIntegral0 ∷ Integral α ⇒ α → ℕ
      fromIntegral0 a | a < 0     = 0
                      | otherwise = fromIntegral a
      p_l ∷ ℕ = maximum ((length ∘ toText ∘ view _1) ⊳ xs')
      pad_p t =
        t ⊕ replicate @𝕋 (p_l - length t) ' '
      p'_l ∷ ℕ = maximum ((length ∘ toText ∘ view (_2 ∘ _1)) ⊳ xs')
      pad_p' t =
        t ⊕ replicate @𝕋 (p'_l - length t) ' '
  let sprint_ppv (p,(p',v)) =
        [fmtT|%T\t%T\t%T|] (pad_p $ toText p) (pad_p' $ toText p')
                           (maybe "" toText v)
  forM_ xs' (say ∘ sprint_ppv)
  return 0

{-| program main entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
-- ?add logging options
-- show all configs (as option)
  let progDesc =
        -- the values of show_configs, etc., will be wrapped as necessary
        let show_configs = fillSep [ toDocTs [ "show the location of configs"] ]
        in vcat [ "manage nix configs for ~home installation\n\nModes:"
                , empty <$$> text "show-configs" ⊞ align show_configs ]
  getArgs ≫ stdMain progDesc parseOptions (myMain {- @UsageParseFPProcIOError -} @UsageParseAesonFPPIOError)

-- that's all, folks! ----------------------------------------------------------
