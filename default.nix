let super = import <nixpkgs> {}; in
(mixin: let pkgs = mixin pkgs super ; in
  pkgs.gerbilPackages-unstable.gerbil-poo)
(pkgs: super: let inherit (super.gerbil-support) overrideGerbilPackage gerbilFilterSource; in
  overrideGerbilPackage "gerbil-poo" (_: gerbilFilterSource ./.) {} super)
