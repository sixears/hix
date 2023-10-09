{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Nix.Error
  ( AsNixDuplicatePkgError(_NixDuplicatePkgError)
  , AsNixError(_NixError)
  , NixDuplicatePkgError
  , NixError(NIX_DUPLICATE_PKG)
  , NixProgramError
  , throwAsNixDuplicatePkgError
  , throwAsNixErrorDuplicatePkg
  ) where

import Base1T

-- aeson-plus --------------------------

import Data.Aeson.Error ( AsAesonError(_AesonError) )

-- base --------------------------------

import GHC.Generics ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq ( NFData )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.Error.FPathError ( AsFPathError(_FPathError) )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError(_CreateProcError) )
import MonadIO.Error.ProcExitError   ( AsProcExitError(_ProcExitError) )

-- stdmain -----------------------------

import StdMain.UsageError ( AsUsageError(_UsageError),
                            UsageParseAesonFPPIOError )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError(_TextualParseError) )
------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Nix.Types ( Pkg )

--------------------------------------------------------------------------------

{-| a duplicate pkg found in a manifest -}
data NixDuplicatePkgError = NixDuplicatePkgError { _pkg       :: Pkg
                                                 , _location  :: AbsFile
                                                 , _callstack :: CallStack
                                                 }
  deriving (Generic, NFData, Show)

----------------------------------------

instance Exception NixDuplicatePkgError

----------------------------------------

instance Eq NixDuplicatePkgError where
  (NixDuplicatePkgError p l _) == (NixDuplicatePkgError p' l' _) =
    p ≡ p' ∧ l ≡ l'

----------------------------------------

instance HasCallstack NixDuplicatePkgError where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

class AsNixDuplicatePkgError ε where
  _NixDuplicatePkgError ∷ Prism' ε NixDuplicatePkgError

--------------------

instance AsNixDuplicatePkgError NixDuplicatePkgError where
  _NixDuplicatePkgError = id

--------------------

instance Printable NixDuplicatePkgError where
  print e = P.text $
    [fmt|Duplicate Pkg %T found in manifest %T|] (_pkg e) (_location e)

------------------------------------------------------------

asNixDuplicatePkgError ∷ (AsNixDuplicatePkgError ε, HasCallStack) ⇒
                         Pkg → AbsFile → ε
asNixDuplicatePkgError pkg mnfst =
  _NixDuplicatePkgError # NixDuplicatePkgError pkg mnfst callStack

----------------------------------------

throwAsNixDuplicatePkgError ∷
  ∀ ε ω η . (AsNixDuplicatePkgError ε, MonadError ε η) ⇒ Pkg → AbsFile → η ω
throwAsNixDuplicatePkgError p m = throwError $ asNixDuplicatePkgError p m

------------------------------------------------------------

data NixError = NIX_DUPLICATE_PKG NixDuplicatePkgError
  deriving (Eq, Generic, NFData, Show)

----------------------------------------

instance Exception NixError

----------------------------------------

instance HasCallstack NixError where
  callstack = lens (\ case (NIX_DUPLICATE_PKG e) → e ⊣ callstack)
                   (\ e cs → case e of (NIX_DUPLICATE_PKG d) →
                                         NIX_DUPLICATE_PKG $ d & callstack ⊢ cs)

----------------------------------------

instance Printable NixError where
  print (NIX_DUPLICATE_PKG e) = print e

----------------------------------------

class AsNixError ε where
  _NixError ∷ Prism' ε NixError

--------------------

instance AsNixError NixError where
  _NixError = id

--------------------

instance AsNixDuplicatePkgError NixError where
  _NixDuplicatePkgError = prism' NIX_DUPLICATE_PKG
                                 (\ case (NIX_DUPLICATE_PKG e) → 𝕵 e {- ; _ → 𝕹 -})

------------------------------------------------------------

asNixErrorDuplicatePkg ∷ (AsNixError ε, HasCallStack) ⇒ Pkg → AbsFile → ε
asNixErrorDuplicatePkg pkg mnfst = _NixError # asNixDuplicatePkgError pkg mnfst

----------------------------------------

throwAsNixErrorDuplicatePkg ∷
  ∀ ε ω η . (AsNixError ε, MonadError ε η) ⇒ Pkg → AbsFile → η ω
throwAsNixErrorDuplicatePkg p m = throwError $ asNixErrorDuplicatePkg p m

------------------------------------------------------------

data NixProgramError = UPAFPPIO_ERROR UsageParseAesonFPPIOError
                     | NIX_ERROR NixError
  deriving (Eq, Generic, NFData)

_UPAFPPIO_ERROR ∷ Prism' NixProgramError UsageParseAesonFPPIOError
_UPAFPPIO_ERROR =
  prism' (\ e → UPAFPPIO_ERROR e)
         (\ case UPAFPPIO_ERROR e → 𝕵 e; _ → 𝕹)

_NIX_ERROR ∷ Prism' NixProgramError NixError
_NIX_ERROR = prism' (\ e → NIX_ERROR e)
                                (\ case NIX_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception NixProgramError

--------------------

instance Show NixProgramError where
  show (NIX_ERROR e)      = show e
  show (UPAFPPIO_ERROR e) = show e

--------------------

instance AsNixError NixProgramError where
  _NixError = _NIX_ERROR

--------------------

instance AsUsageError NixProgramError where
  _UsageError = _UPAFPPIO_ERROR ∘ _UsageError

--------------------

instance AsTextualParseError NixProgramError where
  _TextualParseError = _UPAFPPIO_ERROR ∘ _TextualParseError

--------------------

instance AsAesonError NixProgramError where
  _AesonError = _UPAFPPIO_ERROR ∘ _AesonError

--------------------

instance AsFPathError NixProgramError where
  _FPathError = _UPAFPPIO_ERROR ∘ _FPathError

--------------------

instance AsIOError NixProgramError where
  _IOError = _UPAFPPIO_ERROR ∘ _IOError

--------------------

instance AsCreateProcError NixProgramError where
  _CreateProcError = _UPAFPPIO_ERROR ∘ _CreateProcError

--------------------

instance AsProcExitError NixProgramError where
  _ProcExitError = _UPAFPPIO_ERROR ∘ _ProcExitError

--------------------

instance Printable NixProgramError where
  print (NIX_ERROR   e)    = print e
  print (UPAFPPIO_ERROR e) = print e

--------------------

instance HasCallstack NixProgramError where
  callstack =
    let
      getter (NIX_ERROR   e)    = e ⊣ callstack
      getter (UPAFPPIO_ERROR e) = e ⊣ callstack
      setter (NIX_ERROR   e) cs    = NIX_ERROR (e & callstack ⊢ cs)
      setter (UPAFPPIO_ERROR e) cs = UPAFPPIO_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

-- that's all, folks! ----------------------------------------------------------
