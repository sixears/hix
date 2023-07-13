{-| the `profile-members` command -}
module Nix.Profile.Members
  ( main )
where

import Base1T

-- base --------------------------------

import Data.Maybe  ( fromMaybe )
import System.IO   ( hPutStrLn, stderr )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO.Base   ( getArgs )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( flag, help, long, metavar, short
                                    , strArgument )
import Options.Applicative.Types    ( Parser )

-- parsers -----------------------------

import Text.Parser.Combinators  ( optional )

-- stdmain -----------------------------

import StdMain             ( stdMainNoDR )
import StdMain.UsageError  ( AsUsageError, UsageFPIOTPError )

-- text --------------------------------

import Data.Text     ( intercalate, pack )
import Data.Text.IO  ( putStrLn )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Profile.Manifest  ( Manifest
                             , elementsi, getNameVerPath, readManifest )

--------------------------------------------------------------------------------

data ShowVersion = ShowVersion | NoShowVersion
data ShowIndex   = ShowIndex   | NoShowIndex
data ShowPath    = ShowPath    | NoShowPath

data Options = Options { showVersion ∷ ShowVersion
                       , showIndex   ∷ ShowIndex
                       , showPath    ∷ ShowPath
                       , profileName ∷ 𝕄 𝕋
                       }

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  let version_help  = "show version information, too"
      path_help     = "show store path, too"
      no_index_help = "don't show profile position indices"
  in  Options ⊳ flag NoShowVersion ShowVersion (ю [ short 'v', long "version"
                                                  , help version_help ])
              ⊵ flag ShowIndex     NoShowIndex (ю [ short 'n', long "no-index"
                                                  , help no_index_help ])
              ⊵ flag NoShowPath    ShowPath    (ю [ short 'p', long "path"
                                                  , help path_help ])
              ⊵ optional (strArgument (ю [ metavar "PROFILE-NAME"
                                         , help "profile to enumerate" ]))

------------------------------------------------------------

output_data ∷ Options → Manifest → IO ()
output_data options manifest =
  let pShow ∷ Show α ⇒ α → IO ()
      pShow = hPutStrLn stderr ∘ show

      get_columns i n v p = ю [ case showIndex options of
                                  ShowIndex   → [pack $ show i]
                                  NoShowIndex → []
                              , [toText n]
                              , case showVersion options of
                                  ShowVersion   → [maybe "" toText v]
                                  NoShowVersion → []
                              , case showPath options of
                                  ShowPath   → [toText p]
                                  NoShowPath → []
                              ]

      print_name_ver (i,e) = do
        case getNameVerPath e of
          𝕷 err     → pShow err
          𝕽 (n,v,p) → putStrLn (intercalate "\t" $ get_columns i n v p)

  in forM_ (elementsi manifest) print_name_ver

----------------------------------------

myMain ∷ ∀ ε . (HasCallStack, Printable ε, AsUsageError ε,
                AsTextualParseError ε, AsIOError ε, AsFPathError ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain options = do
  -- Strict' version performs conversion immediately
  readManifest (fromMaybe "" $ profileName options) ≫ \ case
    𝕷 e → liftIO $ hPutStrLn stderr $ show e
    𝕽 stuff → liftIO $ output_data options stuff
  return 0

----------------------------------------

{-| program entry point -}
main ∷ MonadIO μ ⇒ μ ()
main = do
  let progDesc = "list the members of a nix profile" ∷ 𝕋
  getArgs ≫ stdMainNoDR progDesc parseOptions (myMain @UsageFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
