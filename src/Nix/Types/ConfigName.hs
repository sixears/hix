{-# LANGUAGE UnicodeSyntax #-}
module Nix.Types.ConfigName
  ( ConfigName(ConfigName, unConfigName)
  , configDefault
  ) where

import Base1T

-- base --------------------------------

import Data.Ord ( Ord(compare) )

-- fpath -------------------------------

import FPath.Parseable     ( __parse'__ )
import FPath.PathComponent ( PathComponent, pc )

-- parsers -----------------------------

import Text.Parser.Char        ( char, digit, lower )
import Text.Parser.Combinators ( choice )

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types.ToBriefText ( ToBriefText(toT) )

--------------------------------------------------------------------------------

newtype ConfigName = ConfigName { unConfigName :: PathComponent }
  deriving (Eq, Printable, Show)

instance Ord ConfigName where
  compare (ConfigName p) (ConfigName p') = compare (toText p) (toText p')

instance TextualPlus ConfigName where
  textual' = let parse_text = (:) ⊳ lower ⊵ many (choice [lower,digit,char '-'])
             in  ConfigName ∘ __parse'__ ⊳ parse_text

instance ToBriefText ConfigName where
  toT (ConfigName c) = toText c

----------------------------------------

{-| top dir to look for config flakes -}
configDefault ∷ ConfigName
configDefault = ConfigName [pc|default|]

-- that's all, folks! ----------------------------------------------------------
