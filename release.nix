let
  pkgs = import <nixpkgs> {};
in
{ dns = pkgs.haskellPackages.callPackage ./dns.nix {};
}
