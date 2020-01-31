{ ... }:
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-wave-function-collapse";
  buildInputs = [
    niv.niv
    git

    # Elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-live
    elmPackages.elm-test

    # Publishing on Netlify
    nodePackages.npm
    nodejs
  ];
}
