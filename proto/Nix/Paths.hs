module Nix.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

nix :: AbsFile
nix = [absfile|__nix__/bin/nix|]
