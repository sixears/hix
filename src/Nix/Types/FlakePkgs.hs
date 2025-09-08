{-# LANGUAGE UnicodeSyntax #-}
{-| Nix packages, from the view of flakes -}
module Nix.Types.FlakePkgs
  ( FlakePkg
  , FlakePkgs(FlakePkgs)
  , FlakePkgs'(FlakePkgs')
  , HasArchFlakePkgMap(archMap)
  , flakeShowTestInput
  , flakeShowTestMap
  , location
  , packages
  , pkg
  , priority
  , ver
  ) where

import Base1T

-- aeson -------------------------------

import Data.Aeson ( FromJSON(parseJSON), withObject, (.:), (.:?) )

-- base --------------------------------

import GHC.Generics ( Generic )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- text --------------------------------

import Data.Text ( concat, intercalate )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( parseTextM )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types           ( Arch, Pkg, Priority, Ver, unPkgMVer )
import Nix.Types.ConfigDir ( ConfigDir )

--------------------------------------------------------------------------------

data FlakePkg = FlakePkg { _description :: 𝕄 𝕋
                         , _pkg         :: Pkg
                         , _ver         :: 𝕄 Ver
                         , _type        :: 𝕋
                           -- priority isn't really in the flake, it's in our
                           -- own flake.priorities
                         , _priority    :: 𝕄 Priority
                         }
  deriving (Eq, Generic, Show)

pkg ∷ Lens' FlakePkg Pkg
pkg = lens _pkg (\ fp p → (fp { _pkg = p }))

ver ∷ Lens' FlakePkg (𝕄 Ver)
ver = lens _ver (\ fp v → (fp { _ver = v }))

priority ∷ Lens' FlakePkg (𝕄 Priority)
priority = lens _priority (\ fp p → (fp { _priority = p }))

pkgVer ∷ FlakePkg → 𝕋
pkgVer f =
  let p = toText $ f ⊣ pkg
  in  case f ⊣ ver of
        𝓝   → p
        𝓙 v → [fmt|%t-%T|] p v

instance FromJSON FlakePkg where
  parseJSON =
    withObject "FlakePkg" $
    \ v → do
          name ← v .: "name"
          (p,vers) ← unPkgMVer ⊳ parseTextM "PkgMVer" name
          FlakePkg ⊳ v .:? "description" ⊵ return p ⊵ return vers ⊵ v .: "type"
                   -- when reading the flake show output, priority is always 𝓝
                   -- as we read this from flake.priorities
                   ⊵ pure 𝓝
instance Printable FlakePkg where
  print = P.text ∘ pkgVer

------------------------------------------------------------

type Map = Map.Map

newtype FlakePkgs' = FlakePkgs' { unFlakePkgs' :: Map Arch (Map Pkg FlakePkg) }
  deriving (Eq, Generic, Show)

instance FromJSON FlakePkgs' where
  parseJSON = withObject "FlakePkgs'" $ \ v → FlakePkgs' ⊳ v .: "packages"

------------------------------------------------------------

class HasArchFlakePkgMap α where
  archMap ∷ Lens' α (Map Arch (Map Pkg FlakePkg))

instance HasArchFlakePkgMap (Map Arch (Map Pkg FlakePkg)) where
  archMap = id

instance HasArchFlakePkgMap FlakePkgs' where
  archMap = lens unFlakePkgs' (\ _ m → FlakePkgs' m)

--------------------

data FlakePkgs = FlakePkgs { _location :: ConfigDir
                           , _packages :: FlakePkgs'
                           }
  deriving (Show)

location ∷ Lens' FlakePkgs ConfigDir
location = lens _location (\ fp l → fp { _location = l })

packages ∷ Lens' FlakePkgs (Map.Map Arch (Map.Map Pkg FlakePkg))
packages = lens (unFlakePkgs' ∘ _packages)
                (\ p f → (p { _packages = FlakePkgs' f }))

instance HasArchFlakePkgMap FlakePkgs where
  archMap = packages ∘ archMap

instance Printable FlakePkgs where
  print fp =
    let namePkg ∷ Arch → (Map.Map Pkg FlakePkg) → [𝕋]
        namePkg arch pkgs = [ [fmt|packages.%T.%T|] arch p
                              | (p,_) ← Map.toList pkgs ]
    in P.text $
    intercalate " ⫽ " [ intercalate "," (namePkg arch pkgs)
                        | (arch,pkgs) ← Map.toList (fp ⊣ packages) ]

----------------------------------------

flakeShowTestInput ∷ 𝕋
flakeShowTestInput =
  concat [ "{ \"packages\": {"
         , "    \"x86_64-linux\": {"

         , "      \"binutils\": {"
         , "        \"description\": \"MOCK MOCK MOCK\","
         , "        \"name\": \"binutils-wrapper-2.38\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"get-iplayer-config\": {"
         , "        \"name\": \"get-iplayer-config\","
         , "        \"type\": \"derivation\""
         , "      },"

         , "      \"graph-easy\": {"
         , "        \"description\": \"MOCK MOCKETY MOCK\","
         , "        \"name\": \"perl5.34.1-Graph-Easy-0.76\","
         , "        \"type\": \"derivation\""
         , "      }"

         , "    }"
         , "  }"
         , "}"
         ]

--------------------

flakeShowTestMap ∷ Map.Map Pkg FlakePkg
flakeShowTestMap = fromList [ ("binutils",
                               FlakePkg { _description = 𝓙 "MOCK MOCK MOCK"
                                        , _pkg = "binutils-wrapper"
                                        , _ver = Just "2.38"
                                        , _type = "derivation"
                                        , _priority = 𝓝
                                        })
                            , ("get-iplayer-config",
                               FlakePkg { _description = 𝓝
                                        , _pkg = "get-iplayer-config"
                                        , _ver = 𝓝
                                        , _type = "derivation"
                                        , _priority = 𝓝
                                        })
                            , ("graph-easy",
                               FlakePkg { _description = 𝓙 "MOCK MOCKETY MOCK"
                                        , _pkg = "perl5.34.1-Graph-Easy"
                                        , _ver = 𝓙 "0.76"
                                        , _type = "derivation"
                                        , _priority = 𝓝
                                        })
                            ]

-- that's all, folks! ----------------------------------------------------------
