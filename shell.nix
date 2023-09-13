{ system ? builtins.currentSystem, sources ? import ./nix/sources.nix, ghcVersion ? "96" }:

let
  # selectHls = self: super: {
  #   haskell-language-server = super.haskell-language-server.override { supportedGhcVersions = [ "${ghcVersion}" ]; };
  # };
  # pkgs = import sources.nixpkgs { inherit system; overlays = [ selectHls ]; };
  pkgs = import sources.nixpkgs { inherit system; overlays = [ ]; };
  # cabal-docspec = import ./nix/cabal-docspec.nix { inherit pkgs; };
  # stack-wrapped = pkgs.symlinkJoin {
  #   name = "stack";
  #   paths = [ pkgs.stack ];
  #   buildInputs = [ pkgs.makeWrapper ];
  #   postBuild = ''
  #     wrapProgram $out/bin/stack \
  #       --add-flags "\
  #         --nix \
  #         --nix-path=\\"nixpkgs=${pkgs.path}\\"
  #         --nix-shell-file nix/shell-stack.nix \
  #       "
  #   '';
  # };
in with pkgs;

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";
  NIX_PATH = "nixpkgs=${pkgs.path}";

  buildInputs = [
    bashInteractive
    haskell.compiler."ghc${ghcVersion}"
    cabal-install
    gnumake
    # stack-wrapped
    # nix
    # haskell-language-server
  ];
}
