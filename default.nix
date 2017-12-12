{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  userPackages = import <userpackages>;
  myemacs =
    with pkgs.emacsPackages; with pkgs.emacsPackagesNg; pkgs.emacsWithPackages
      [ ace-jump-mode haskell-mode helm-projectile haskellMode magit paredit
        undo-tree ];
        # TODO Includes nix/web.nix. Should reduce duplication.
  myhaskell = pkgs.haskellPackages.ghcWithPackages (p: with p;
    [ attoparsec
      linear
      cabal-install
    ]);
in pkgs.stdenv.mkDerivation {
  name = "momentoftop";
  buildInputs = [ myemacs myhaskell pkgs.imagemagick ];
}
