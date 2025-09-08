{-# LANGUAGE UnicodeSyntax #-}
{-| Interface to `nix`, the executable -}
module Nix.NixExe
  ( nixBuild
  , nixFlakeShow
  , nixProfileInstall
  , nixProfileRemove
  ) where

import Base1T

-- aeson-plus --------------------------

import Data.Aeson       ( eitherDecodeStrict' )
import Data.Aeson.Error ( AsAesonError, throwAsAesonError )
import Data.List        ( sort )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

-- env-plus ----------------------------

import Env.Types ( EnvModFrag, ә, ӭ )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.AsFilePath       ( filepath )
import FPath.Error.FPathError ( AsFPathError )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog )

-- mockio ------------------------------

import MockIO.DoMock ( HasDoMock )

-- mockio-log --------------------------

import MockIO.Log             ( MockIOClass )
import MockIO.Log.MonadReader ( notice, warn )

-- mockio-plus -------------------------

import MockIO.Process               ( ꙩ )
import MockIO.Process.MLCmdSpec     ( MLCmdSpec, ToMLCmdSpec, mock_value )
import MockIO.Process.OutputDefault ( OutputDefault )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.Process.ExitStatus    ( ExitStatus, evOK )
import MonadIO.Process.MakeProc      ( MakeProc )
import MonadIO.Process.OutputHandles ( OutputHandles )
import MonadIO.Process.ToMaybeTexts  ( ToMaybeTexts )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader )

-- text --------------------------------

import Data.Text          qualified as T
import Data.Text.Encoding ( encodeUtf8 )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Paths qualified as Paths

import Nix.Types             ( Pkg, Priority(unPriority), ProfileDir,
                               RemoteState, remoteArgs )
import Nix.Types.ConfigDir   ( ConfigDir(unConfigDir) )
import Nix.Types.FlakePkgs   ( FlakePkgs(FlakePkgs), flakeShowTestInput )
import Nix.Types.ToBriefText ( ToBriefText(toT) )

--------------------------------------------------------------------------------

nixDo ∷ ∀ ε α δ ζ φ μ .
        (MonadIO μ, Foldable φ, MonadReader δ μ, HasDoMock δ,
         AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
         Printable ε, MonadError ε μ,
         MakeProc ζ, OutputDefault α, ToMaybeTexts α, OutputHandles ζ α,
         ToMLCmdSpec (AbsFile,[𝕋],[EnvModFrag]) α,
         ToMLCmdSpec (AbsFile,[𝕋],[EnvModFrag],(MLCmdSpec α → MLCmdSpec α)) α,
         MonadLog (Log MockIOClass) μ) ⇒
        (𝕄 (MLCmdSpec α → MLCmdSpec α)) → φ 𝕋 → μ α

nixDo 𝓝 args = snd ⊳ ꙩ (Paths.nix, toList args, [ӭ (ә "NIX_CONFIG")])
nixDo (𝓙 mock_set) args =
  snd ⊳ ꙩ (Paths.nix, toList args, [ӭ (ә "NIX_CONFIG")], mock_set)

----------------------------------------

mkTargets ∷ (Functor φ, Printable τ) ⇒ ConfigDir → φ τ → φ 𝕋
mkTargets config_dir attr_paths =
  [fmt|%T#%T|] (unConfigDir config_dir) ⊳ attr_paths

----------------------------------------

msg ∷ ∀ τ δ φ η . (MonadIO η, Foldable φ, Printable τ, ToBriefText τ,
                   HasDoMock δ, MonadReader δ η, MonadLog (Log MockIOClass) η) ⇒
      𝕋 → τ → φ Pkg → η ()
msg verb object pkgs = do
  let names = sort $ toText ⊳ toList pkgs
  warn $ [fmt|%t (%t): %L|] verb (toT object) names
  notice $ [fmt|%t: (%T) %L|] verb object pkgs

----------------------------------------

nixBuild ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                      AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                      AsProcExitError ε, Printable ε, MonadError ε μ,
                      MonadLog (Log MockIOClass) μ) ⇒
           RemoteState → ConfigDir → NonEmpty Pkg → μ ()
nixBuild r config_dir attr_paths = do
  msg "building" config_dir attr_paths
  let targets = mkTargets config_dir attr_paths
  nixDo 𝓝 $ ю [ [ "build", "--log-format", "bar-with-logs", "--no-link" ]
              , remoteArgs r, toList targets ]

----------------------------------------

nixProfileRemove ∷ ∀ ε δ μ . (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                              AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                              AsProcExitError ε, Printable ε, MonadError ε μ,
                              MonadLog (Log MockIOClass) μ) ⇒
                   RemoteState → ProfileDir → [Pkg] → μ ()
nixProfileRemove _ _ [] = return ()
nixProfileRemove r profile pkgs = do
  msg "removing" profile pkgs
  nixDo 𝓝 $ ю [ [ "profile", "remove", "--verbose", "--profile", toText profile]
              , remoteArgs r, toText ⊳ pkgs ]

----------------------------------------

nixProfileInstall ∷ ∀ ε δ μ .
                    (MonadIO μ, MonadReader δ μ, HasDoMock δ,
                     AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                     AsProcExitError ε, Printable ε, MonadError ε μ,
                     MonadLog (Log MockIOClass) μ) ⇒
                    RemoteState → ConfigDir → ProfileDir
                  → 𝕄 Priority → NonEmpty Pkg → μ ()
nixProfileInstall r config_dir profile prio_m pkgs = do
  let verb = maybe "" [fmt| «prio %T»|] prio_m
  msg ("installing" ◇ verb) (config_dir, profile) (NonEmpty.sort pkgs)
  let targets = mkTargets config_dir pkgs
  let extra_args = maybe [] (\ p → ["--priority", [fmt|%d|] (unPriority p)])
                         prio_m
  nixDo 𝓝 $ ю [ [ "profile", "install", "--profile", toText profile ]
              , remoteArgs r
              , extra_args, toList targets ]

----------------------------------------

{-| nix flake show #flake.  Note that this doesn't handle priorities. -}
nixFlakeShow ∷ ∀ ε δ μ .
               (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                AsTextualParseError ε, AsProcExitError ε, AsAesonError ε,
                Printable ε, MonadError ε μ,
                MonadLog (Log MockIOClass) μ) ⇒
               RemoteState → ConfigDir → μ FlakePkgs
nixFlakeShow r d = do
  let eAsAesonError ∷ (Printable τ,AsAesonError ε,MonadError ε η) ⇒ 𝔼 τ β → η β
      eAsAesonError = either throwAsAesonError return
      mock_set ∷ MLCmdSpec 𝕋 → MLCmdSpec 𝕋
      mock_set = let mock_val ∷ (ExitStatus, 𝕋) = (evOK, flakeShowTestInput)
                 in  (& mock_value ⊢ mock_val)
      args     = ю [ ["flake", "show", "--json" ]
                   , remoteArgs r
                   , [ T.pack $ (unConfigDir d) ⫥ filepath ] ]
  flake_show ← nixDo (𝓙 mock_set) args
  eAsAesonError (FlakePkgs d ⊳ eitherDecodeStrict' (encodeUtf8 flake_show))

-- that's all, folks! ----------------------------------------------------------
