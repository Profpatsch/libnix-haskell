{ pkgs ? import <nixpkgs> {}}:

pkgs.haskellPackages.shellFor {
  packages = hps: [
    (hps.callCabal2nix "libnix" (pkgs.nix-gitignore.gitignoreSource [".git"] ./.) {})
  ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
