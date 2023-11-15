{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.ConfigDir
  ( ConfigDir(ConfigDir, unConfigDir)
  , allConfigDirs
  , allConfigNames
  , configDir
  , configDirFromAbs
  , configDirName
  , configNameFromDir
  , configTop
  ) where

import Base1T

import Prelude ( error )

-- base --------------------------------

import Data.List ( any )

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD, AbsF) )
import FPath.AbsDir           ( AbsDir )
import FPath.AbsFile          ( AbsFile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.Basename         ( Basename, basename )
import FPath.Dir              ( DirAs )
import FPath.Dirname          ( dirname )
import FPath.Error.FPathError ( AsFPathError )
import FPath.Parseable        ( Parseable(parse) )
import FPath.PathComponent    ( PathComponent )
import FPath.RelDir           ( reldir )
import FPath.RelFile          ( relfile )
import FPath.RelType          ( RelType )
import FPath.ToDir            ( ToDir(toDir) )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock(NoMock), HasDoMock )
import MockIO.IOClass ( HasIOClass )

-- mockio-plus -------------------------

import MockIO.Directory ( lsdir', subdirs )

-- monadio-plus ------------------------

import MonadIO.FPath ( pResolve )
import MonadIO.FStat ( isDir )
import MonadIO.User  ( homePath )

-- mono-traversable --------------------

import Data.MonoTraversable ( Element, MonoFoldable, otoList )

-- safe --------------------------------

import Safe ( lastMay )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types.ConfigName  ( ConfigName(ConfigName, unConfigName) )
import Nix.Types.ToBriefText ( ToBriefText(toT) )

--------------------------------------------------------------------------------

newtype ConfigDir = ConfigDir { unConfigDir :: AbsDir }
  deriving (Printable, Show)

instance ToBriefText ConfigDir where
  toT = toT ∘ configDirName

------------------------------------------------------------

{-| top dir to look for config flakes -}
configTop ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            μ AbsDir
configTop = homePath [reldir|nix/|]

----------------------------------------

{-| list of config directories; that is, dirs in `configTop` that contain a
    @flake.nix@ -}
allConfigDirs ∷ (MonadIO μ,
              HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ,
              AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
             μ [ConfigDir]
allConfigDirs = do
  config_top  ← configTop
  let has_flake ∷ (MonadIO μ,
                   AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,
                   HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                  AbsDir → μ 𝔹
      has_flake d  = do
        (fs,_) ← lsdir' @_ @AbsFile Informational d NoMock
        return $ any (\ (fn, _) → [relfile|flake.nix|] ≡ basename fn) fs
  ConfigDir ⊳⊳ (subdirs Informational config_top NoMock ≫ filterM has_flake)

----------------------------------------

allConfigNames ∷ (MonadIO μ,
                  HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ,
                  AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
                 μ [ConfigName]
allConfigNames = configDirName ⊳⊳ allConfigDirs

----------------------------------------

configDirName ∷ ConfigDir → ConfigName
configDirName = configNameFromDir ∘ unConfigDir

----------------------------------------

configNameFromDir ∷ (DirAs δ, Element (RelType δ) ~ PathComponent,
                     MonoFoldable (RelType δ), Basename δ) ⇒ δ → ConfigName
configNameFromDir d = case lastMay ∘ otoList $ basename d of
                        𝕹   → error $ [fmt|could not find ConfigName of %T|] d
                        𝕵 p → ConfigName p

----------------------------------------

configDir ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
            ConfigName → μ ConfigDir
configDir p = ConfigDir ⊳ ((⫻ fromList [unConfigName p]) ⊳ configTop)

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

-- that's all, folks! ----------------------------------------------------------
