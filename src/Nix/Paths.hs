module Nix.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

nix :: AbsFile
nix = [absfile|/nix/store/7q9ll9pjrdfdb3qyfza2bzrk829izk9s-nix-2.28.4/bin/nix|]
