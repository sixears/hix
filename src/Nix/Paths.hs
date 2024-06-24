module Nix.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

nix :: AbsFile
nix = [absfile|/nix/store/j7rp0y3ii1w3dlbflbxlv4g7hbaaz3bs-nix-2.18.2/bin/nix|]
