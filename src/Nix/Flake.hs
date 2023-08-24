{-# LANGUAGE UnicodeSyntax #-}
module Nix.Flake
  ( FlakePkg
  , FlakePkgs
  , flakeShow
  , forMX86_64Pkg
  , forMX86_64Pkg_
  , forX86_64Pkg
  , pkg
  , ver
  , x86_64
  , x86_64_
  ) where

import Prelude ( undefined )

import Base1T

-- aeson -------------------------------

import Data.Aeson ( FromJSON(parseJSON), defaultOptions, eitherDecodeStrict',
                    fieldLabelModifier, genericParseJSON, withObject, (.:),
                    (.:?) )

import Data.Aeson.Types as AesonTypes

-- aeson-plus --------------------------

import Data.Aeson.Error ( AesonError, AsAesonError(_AesonError),
                          throwAsAesonError )

-- base --------------------------------

import Data.Maybe   ( fromMaybe )
import Data.Tuple   ( uncurry )
import GHC.Generics ( Generic )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir )
import FPath.AsFilePath       ( AsFilePath, filepath )
import FPath.Error.FPathError ( AsFPathError(_FPathError) )

-- lens --------------------------------

import Control.Lens.At     ( at )
import Control.Lens.Getter ( (^.) )
import Control.Lens.Lens   ( Lens' )

-- log-plus ----------------------------

import Log ( Log, logIO )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- mockio-log --------------------------

import MockIO.Log ( HasDoMock, MockIOClass, doMock, logResult, mkIOL, mkIOLME,
                    mkIOLMER )

-- mockio-plus -------------------------

import MockIO.Process           ( ꙫ )
import MockIO.Process.MLCmdSpec ( MLCmdSpec, ToMLCmdSpec, mock_value )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError(_CreateProcError) )
import MonadIO.Error.ProcExitError   ( AsProcExitError(_ProcExitError) )
import MonadIO.Process.ExitStatus    ( ExitStatus, evOK )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( tindex )

-- mtl ---------------------------------

import Control.Monad.Except ( throwError )
import Control.Monad.Reader ( MonadReader, runReaderT )

-- text --------------------------------

import Data.Text          ( concat, pack, unpack )
import Data.Text.Encoding ( encodeUtf8 )

-- textual-plus ------------------------

import TextualPlus qualified

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Paths qualified as Paths

import Nix.Types ( Arch, Pkg, Ver, pkgRE, x86_64Linux )

--------------------------------------------------------------------------------

customOptions ∷ AesonTypes.Options
customOptions =
  let dropLeadingUnderscore ('_' : s) = s
      dropLeadingUnderscore s         = s
  in  defaultOptions { fieldLabelModifier = dropLeadingUnderscore }

------------------------------------------------------------

data FlakePkg = FlakePkg { _description :: 𝕄 𝕋
                         , _pkg         :: Pkg
                         , _ver         :: 𝕄 Ver
                         , _type        :: 𝕋
                         }
  deriving (Generic, Show)

pkg ∷ Lens' FlakePkg Pkg
pkg = lens _pkg (\ fp p → (fp { _pkg = p }))

ver ∷ Lens' FlakePkg (𝕄 Ver)
ver = lens _ver (\ fp v → (fp { _ver = v }))

instance FromJSON FlakePkg where
  parseJSON = withObject "FlakePkg" $
    \ v → do
          name ← v .: "name"
          (pkg,ver) ← TextualPlus.parseT pkgRE "FlakePkg" (unpack name)
          FlakePkg ⊳ v .:? "description"
                       ⊵ return pkg ⊵ return ver
                       ⊵ v .: "type"

------------------------------------------------------------

newtype FlakePkgs = FlakePkgs { _packages :: Map.Map Arch (Map.Map Pkg FlakePkg) }
  deriving (Generic, Show)

instance FromJSON FlakePkgs where
  parseJSON = genericParseJSON customOptions

----------------------------------------

-- packages ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
-- packages = lens _packages (\ p f → (FlakePkgs { _packages = f }))
packages ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
packages = lens _packages (\ p f → (FlakePkgs { _packages = f }))

xx ∷ Arch → Lens' FlakePkgs (𝕄 (Map.Map Pkg FlakePkg))
xx a =
  let pkgs ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
      pkgs = lens _packages (\ p f → (FlakePkgs { _packages = f }))
  in  pkgs ∘ (at a)

yy ∷ Arch → Lens' FlakePkgs (Map.Map Pkg FlakePkg)
yy a =
  let pkgs ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
      pkgs = lens _packages (\ p f → (FlakePkgs { _packages = f }))
      f1 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg
      f1 fps = fromMaybe Map.empty $ a `Map.lookup` fps
      f2 ∷ Map.Map Arch (Map.Map Pkg FlakePkg) → Map.Map Pkg FlakePkg → Map.Map Arch (Map.Map Pkg FlakePkg)
      f2 fps new = Map.insert a new fps
  in  pkgs ∘ (lens f1 f2 {- (\ map → Map.findWithDefault _ _ map) -})

----------------------------------------

x86_64 ∷ FlakePkgs → 𝕄 (Map.Map Pkg FlakePkg)
-- x86_64 (f ∷ FlakePkgs) = (f ⊣ packages) ⊣ at x86_64Linux
x86_64 f = (f ⊣ packages) ⊣ at x86_64Linux

-- x86_64_ ∷ Lens' FlakePkgs (𝕄 (Map.Map Pkg FlakePkg))
-- x86_64_ = xx x86_64Linux
x86_64_ ∷ Lens' FlakePkgs (Map.Map Pkg FlakePkg)
x86_64_ = yy x86_64Linux

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

{-| nix flake show #flake -}
flakeShow ∷ ∀ ε δ μ .
            (MonadIO μ, HasDoMock δ, MonadReader δ μ,
             AsIOError ε, AsFPathError ε, AsCreateProcError ε,
             AsProcExitError ε, AsAesonError ε, Printable ε,
             MonadError ε μ,
             MonadLog (Log MockIOClass) μ) ⇒
            AbsDir → μ (𝔼 𝕊 FlakePkgs)
flakeShow d = do
  let mock_set ∷ MLCmdSpec 𝕋 → MLCmdSpec 𝕋
      mock_set = let mock_val ∷ (ExitStatus, 𝕋) = (evOK, flakeShowTestInput)
                 in  (& mock_value ⊢ mock_val)
      args     = ["flake", "show", "--json", pack $ d ⫥ filepath]
  (_,flake_show) ← ꙫ (Paths.nix, args, mock_set)

  return $ eitherDecodeStrict' (encodeUtf8 flake_show)

-- that's all, folks! ----------------------------------------------------------
