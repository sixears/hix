{-# LANGUAGE UnicodeSyntax #-}
{-| the `profile-members` command -}
module Nix.Profile.Members
  ( main
  ) where

import Base1T

-- base --------------------------------

import System.IO  ( hPutStrLn, stderr )

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, Severity(Informational) )

-- mockio ------------------------------

import MockIO.DoMock ( DoMock(NoMock) )

-- mockio-log --------------------------

import MockIO.MockIOClass ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO.Base ( getArgs )

-- mtl ---------------------------------

import Control.Monad.Reader ( runReaderT )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( flag, help, long, metavar, short,
                                     strArgument )
import Options.Applicative.Types   ( Parser )

-- parsers -----------------------------

import Text.Parser.Combinators ( optional )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( AsUsageError, UsageFPIOTPError )

-- text --------------------------------

import Data.Text    ( intercalate, pack )
import Data.Text.IO ( putStrLn )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Profile.Manifest ( elementsi, readManifest )
import Nix.Types.Manifest   ( Manifest, getNameVerPathPrio )

--------------------------------------------------------------------------------

data ShowVersion = ShowVersion | NoShowVersion
data ShowIndex = ShowIndex | NoShowIndex
data ShowPath = ShowPath | NoShowPath
data ShowPriority = ShowPriority | NoShowPriority

data Options = Options { showVersion  :: ShowVersion
                       , showIndex    :: ShowIndex
                       , showPath     :: ShowPath
                       , showPriority :: ShowPriority
                       , profileName  :: 𝕄 𝕋
                       }

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  let version_help  = "show version information, too"
      path_help     = "show store path, too"
      priority_help = "show priority, too"
      no_index_help = "don't show profile position indices"
  in  Options ⊳ flag NoShowVersion  ShowVersion  (ю [ short 'V', long "version"
                                                    , help version_help ])
              ⊵ flag ShowIndex      NoShowIndex  (ю [ short 'n', long "no-index"
                                                    , help no_index_help ])
              ⊵ flag NoShowPath     ShowPath     (ю [ short 'P', long "path"
                                                    , help path_help ])
              ⊵ flag NoShowPriority ShowPriority (ю [ short 'R', long "priority"
                                                    , help path_help ])
              ⊵ optional (strArgument (ю [ metavar "PROFILE-NAME"
                                         , help priority_help ]))

------------------------------------------------------------

output_data ∷ Options → Manifest → IO ()
output_data options manifest =
  let pShow ∷ Show α ⇒ α → IO ()
      pShow = hPutStrLn stderr ∘ show

      get_columns i n v p r = ю [ case showIndex options of
                                    ShowIndex   → [pack $ show i]
                                    NoShowIndex → []
                                , [toText n]
                                , case showVersion options of
                                    ShowVersion   → [maybe "" toText v]
                                    NoShowVersion → []
                                , case showPath options of
                                    ShowPath   → [toText p]
                                    NoShowPath → []
                                , case showPriority options of
                                    ShowPriority   → [maybe "" toText r]
                                    NoShowPriority → []
                                ]

      print_name_ver (i,e) = do
        case getNameVerPathPrio @TextualParseError e of
          𝓛 err           → pShow err
          𝓡 𝓝             → return ()
          𝓡 (𝓙 (n,v,p,r)) → putStrLn (intercalate "\t" $ get_columns i n v p r)

  in forM_ (elementsi manifest) print_name_ver

----------------------------------------

myMain ∷ ∀ ε . (HasCallStack, Printable ε, AsUsageError ε,
                AsTextualParseError ε, AsIOError ε, AsFPathError ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain options = do
  -- Strict' version performs conversion immediately
  flip runReaderT NoMock $ readManifest Informational (profileName options) ≫ \ case
    𝓛 e     → liftIO $ hPutStrLn stderr $ show e
    𝓡 stuff → liftIO $ output_data options stuff
  return 0

----------------------------------------

{-| program entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
  let progDesc = "list the members of a nix profile" ∷ 𝕋
  getArgs ≫ stdMainNoDR progDesc parseOptions (myMain @UsageFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
