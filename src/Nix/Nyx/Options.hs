{-# LANGUAGE UnicodeSyntax #-}
module Nix.Nyx.Options
  ( Configs(..)
  , Mode(..)
  , Options
  , Packages(..)
  , mode
  , parseOptions
  , remote_state
  ) where

import Base1T
import Prelude ( Monoid )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( command, eitherReader, flag, flag', help,
                                     info, long, option, progDesc, short,
                                     strArgument, subparser )
import Options.Applicative.Types   ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

-- parsers -----------------------------

import Text.Parser.Char        ( text )
import Text.Parser.Combinators ( sepBy1 )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parseTextual )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types ( ConfigName, Pkg(Pkg),
                   RemoteState(FullyConnected, Isolated, Remote) )

--------------------------------------------------------------------------------

data Packages = AllPackages
              | SomePackages (NonEmpty Pkg)

------------------------------------------------------------

data Configs = AllConfigs
             | SomeConfigs [ConfigName]

------------------------------------------------------------

data Mode = ModeListPkgs Configs
          | ModeListConfigs
          | ModeListConfigNames
          | ModeInstall [ConfigName] Packages

------------------------------------------------------------

data Options = Options { _remote_state :: RemoteState
                       , _mode         :: Mode
                       }

remote_state ∷ Lens' Options RemoteState
remote_state = lens _remote_state (\ o rs → o { _remote_state = rs })

mode ∷ Lens' Options Mode
mode = lens _mode (\ o m → o { _mode = m })

------------------------------------------------------------

newtype ConfigNames = ConfigNames { unConfigNames :: [ConfigName] }
  deriving (Monoid, Semigroup)

instance Printable ConfigNames where
  print (ConfigNames cs) = P.text $ [fmt|%L|] cs

instance TextualPlus ConfigNames where
  textual' = ConfigNames ⊳ sepBy1 textual' (text ",")

------------------------------------------------------------

{-| cmdline options parser -}
parseOptions ∷ Parser Options
parseOptions =
  let
    configs_option ∷ Parser [ConfigName] =
      unConfigNames ∘ ю ⊳
        many (option @ConfigNames (eitherReader parseTextual)
                                   (ю [ short 'c', long "config"
                                      , help "select config to use" ]))

    configs_option' ∷ Parser Configs =
      ( SomeConfigs ⊳ configs_option ∤ flag' AllConfigs (ю [ short 'A', long "all-configs"] ) )
    install_parser ∷ Parser Mode
    install_parser =
      ModeInstall ⊳ configs_option
                  ⊵ (  (SomePackages ⊳ parseNE (Pkg ⊳ strArgument (help "package")))
                     ∤ flag' AllPackages (ю [ short 'a'
                                            , help "all packages" ]))

  in
    Options ⊳ ( flag FullyConnected Remote
                     (ю [ short 'r', long "remote"
                        , help "disconnected from sixears network" ])
              ∤ flag' Isolated (ю [ short 'R', long "isolated"
                                  , help "disconnected from all networks" ]))
            ⊵ subparser (ю [ command "list-config-dirs"
                                     (info (pure ModeListConfigs)
                                      (progDesc "list config directories"))
                           , command "list-config-names"
                                     (info (pure ModeListConfigNames)
                                      (progDesc "list config names"))
                           , command "list-packages"
                                     (info (ModeListPkgs ⊳ configs_option')
                                      (progDesc "list packages"))
                           , command "install"
                                     (info install_parser
                                      (progDesc "install one or more packages"))
                           ])

-- that's all, folks! ----------------------------------------------------------
