{-# LANGUAGE UnicodeSyntax #-}
module Nix
  ( nixDo
  ) where

import Base1T

-- base --------------------------------

import Data.Foldable ( Foldable )

-- env-plus ----------------------------

import Env.Types ( EnvModFrag, ә, ӭ )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.Error.FPathError ( AsFPathError )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock ( DoMock(NoMock), HasDoMock(doMock) )

-- mockio-log --------------------------

import MockIO.Log ( MockIOClass, infoIO, warnIO )

-- mockio-plus -------------------------

import MockIO.Process               ( ꙩ )
import MockIO.Process.MLCmdSpec     ( MLCmdSpec, ToMLCmdSpec )
import MockIO.Process.OutputDefault ( OutputDefault )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.Process.MakeProc      ( MakeProc )
import MonadIO.Process.OutputHandles ( OutputHandles )
import MonadIO.Process.ToMaybeTexts  ( ToMaybeTexts )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Paths qualified as Paths

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

nixDo 𝕹 args = snd ⊳ ꙩ (Paths.nix, toList args, [ӭ (ә "NIX_CONFIG")])
nixDo (𝕵 mock_set) args =
  snd ⊳ ꙩ (Paths.nix, toList args, [ӭ (ә "NIX_CONFIG")], mock_set)


-- that's all, folks! ----------------------------------------------------------
