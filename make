#!/home/martyn/bin/bash

pkgs_nix=$(dirname $( dirname $( realpath $(type -p nix) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  perl -plE "s{__nix__}{$pkgs_nix}g" "$f" > "$t"
done
