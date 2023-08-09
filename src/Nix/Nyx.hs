{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE UnicodeSyntax       #-}

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

-- base --------------------------------

import Data.Foldable    ( Foldable )
import Data.Function    ( flip )
import Data.Functor     ( Functor )
import Data.List        ( any, filter, maximum, unzip3 )
import Data.Traversable ( traverse )
import GHC.Generics     ( Generic )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- data-textual ------------------------

import Data.Textual ( Parsed(Malformed, Parsed), Textual(textual) )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, absdir )
import FPath.AbsFile          ( AbsFile, absfile )
import FPath.AppendableFPath  ( AppendableFPath, AppendableFPathD,
                                AppendableFPathF, (⫻) )
import FPath.AsFilePath       ( AsFilePath, filepath )
import FPath.Basename         ( basename )
import FPath.Dir              ( DirAs )
import FPath.DirType          ( DirType )
import FPath.Error.FPathError ( AsFPathError(_FPathError) )
import FPath.Parseable        ( parse )
import FPath.PathComponent    ( PathComponent )
import FPath.RelDir           ( RelDir, reldir )
import FPath.RelFile          ( RelFile, relfile )
import FPath.ToDir            ( ToDir )
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
import MockIO.Process               ( ꙫ )
import MockIO.Process.MLCmdSpec     ( MLCmdSpec, ToMLCmdSpec, mock_value )
import MockIO.Process.OutputDefault ( OutputDefault )

-- monaderror-io -----------------------

import MonadError.IO.Error ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                       ( say )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError(_CreateProcError) )
import MonadIO.Error.ProcExitError   ( AsProcExitError(_ProcExitError) )
import MonadIO.Process.ExitInfo      ( ExitInfo )
import MonadIO.Process.ExitStatus    ( ExitStatus, evOK )
import MonadIO.Process.MakeProc      ( MakeProc )
import MonadIO.Process.OutputHandles ( OutputHandles )
import MonadIO.Process.ToMaybeTexts  ( ToMaybeTexts )
import MonadIO.User                  ( getUserName', homePath )

-- mtl ---------------------------------

import Control.Monad.Except ( throwError )
import Control.Monad.Reader ( MonadReader, runReaderT )

-- natural -----------------------------

import Natural ( length, replicate )

-- optparse-applicative ----------------

import Options.Applicative.Builder     ( argument, auto, command, info, metavar,
                                         progDesc, subparser )
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
                            UsageParseAesonFPPIOError, UsageParseFPProcIOError )

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

import Data.Aeson.Error ( AesonError, AsAesonError(_AesonError),
                          throwAsAesonError )

import Nix.Flake ( FlakePkgs, forMX86_64Pkg, forMX86_64Pkg_, forX86_64Pkg, pkg,
                   ver, x86_64, x86_64_ )
import Nix.Types ( Arch, Pkg, Ver, pkgRE )

--------------------------------------------------------------------------------

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

data Mode = ModeListPkgs | ModeListConfigs

instance Printable Mode where

instance Textual Mode where
  textual = string "list-packages"    ⋫ pure ModeListPkgs
          ∤ string "list-config-dirs" ⋫ pure ModeListConfigs

-- XXX This should be a sub-command or similar

data Options = Options { mode :: Mode
                       }

----------------------------------------

{-| cmdline options parser -}
parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ subparser ( command "list-packages" (info (pure ModeListPkgs) (progDesc "list packages")) ⊕ command "list-config-dirs" (info (pure ModeListConfigs) (progDesc "list config directories")))

--  argument readT (metavar "MODE")

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

flakeShowTestInput ∷ 𝕋
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

----------------------------------------

{-| nix flake show #flake -}
nix_flake_show ∷ ∀ ε δ α ξ ζ μ .
                 (MonadIO μ, DirAs α,
                  AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                  AsProcExitError ε, Printable ε, MonadError ε μ,
                  OutputDefault ξ, ToMaybeTexts ξ, OutputHandles ζ ξ,
                  ToMLCmdSpec (AbsFile, [𝕋], MLCmdSpec 𝕋 → MLCmdSpec 𝕋) ξ,
                  MakeProc ζ,
                  HasDoMock δ, MonadReader δ μ,
                  MonadLog (Log MockIOClass) μ) ⇒
                 α → μ (ExitInfo, ξ)
nix_flake_show flake =
  let mock_set ∷ MLCmdSpec 𝕋 → MLCmdSpec 𝕋
      mock_set = let mock_val ∷ (ExitStatus, 𝕋) = (evOK, flakeShowTestInput)
                 in  (& mock_value ⊢ mock_val)
      args     = ["flake", "show", "--json", pack $ flake ⫥ filepath]
  in  ꙫ (Paths.nix, args, mock_set)

nix_flake_show' ∷ ∀ ε δ μ .
                  (Printable ε, MonadError ε μ, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                   AsProcExitError ε,  MonadIO μ, HasDoMock δ, MonadReader δ μ,
                   MonadLog (Log MockIOClass) μ) ⇒
                  AbsDir → μ (ExitInfo, 𝕋)
nix_flake_show' = nix_flake_show

nix_flake_show'' ∷ ∀ ε δ μ .
                   (Printable ε, MonadError ε μ, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                    AsProcExitError ε,  MonadIO μ, HasDoMock δ, MonadReader δ μ,
                    MonadLog (Log MockIOClass) μ) ⇒
                   AbsDir → μ (𝔼 𝕊 FlakePkgs)
nix_flake_show'' d = snd ⊳ nix_flake_show' d ≫ \ s → return $ eitherDecodeStrict' (encodeUtf8 s)

nix_flake_show''' ∷ ∀ ε δ μ .
                    (Printable ε, MonadError ε μ, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                     AsProcExitError ε, AsAesonError ε,  MonadIO μ, HasDoMock δ, MonadReader δ μ,
                     MonadLog (Log MockIOClass) μ) ⇒
                    AbsDir → μ [(𝕋,𝕋,𝕋)]

nix_flake_show''' d = nix_flake_show'' d ≫ \ case
  𝕽 pkgs → return ∘ Map.foldMapWithKey (\ p fp → [(toText $ p, toText $ fp ⊣ pkg, maybe "" toText $ fp ⊣ ver)]) $ pkgs ⊣ x86_64_
  𝕷 e    → throwAsAesonError e


natNeg ∷ ℕ → ℕ → ℕ
natNeg x y = if x ≥ y then x - y else 0

(⊖) ∷ ℕ → ℕ → ℕ
(⊖) = natNeg

data NumSign = SignPlus | SignMinus

unNegate ∷ ℤ → (NumSign,ℕ)
unNegate n | n < 0     = (SignMinus, fromIntegral $ abs n)
           | otherwise = (SignPlus,  fromIntegral n)

pad_t ∷ ℤ → 𝕋 → 𝕋
pad_t (unNegate → (SignMinus,n)) t = replicate @𝕋 (n ⊖ length t) ' ' ⊕ t
pad_t (unNegate → (SignPlus, n)) t = t ⊕ replicate @𝕋 (n ⊖ length t) ' '

myMain ∷ ∀ ε . (HasCallStack,
                AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε, AsAesonError ε, Printable ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = flip runReaderT do_mock $
  case mode opts of
    ModeListPkgs -> do

      allConfigs ≫ mapM_ say

      xs ∷ [(𝕋,𝕋,𝕋)] ← nix_flake_show''' [absdir|/home/martyn/nix/default/|]
-- XXX turn this into a columnify function

--  let pads = (PadLeft,PadLeft,PadRight)

      let lengths ∷ (ℤ,ℤ,ℤ) =
           (& _1 ⊧ fromIntegral) $
           (& _2 ⊧ fromIntegral) $
           (& _3 ⊧ (*(-1)) ∘ fromIntegral) $
           (unzip3 xs) & each ⊧ (\ ys → maximumDef 0 $ length ⊳ ys)

      let sprint_ppv zs =
           let zip3TWith f (a,b,c) (x,y,z) = ((f a x), (f b y), (f c z))
           in  intercalate "\t" $ (^.. each) $ (zip3TWith pad_t lengths zs)
      forM_ xs (say ∘ sprint_ppv)
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
