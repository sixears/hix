module Nix.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

nix :: AbsFile
nix = [absfile|/nix/store/pfaw5zpqfang1w1ms61lw8q60sg7fzxy-nix-2.12.0/bin/nix|]
