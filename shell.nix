{ system ? builtins.currentSystem, sources ? import ./nix/sources.nix, ghcVersion ? "962" }:

let
  selectHls = self: super: {
    haskell-language-server = super.haskell-language-server.override { supportedGhcVersions = [ "${ghcVersion}" ]; };
  };
  pkgs = import sources.nixpkgs { inherit system; overlays = [ selectHls ]; };
  cabal-docspec = import ./nix/cabal-docspec.nix { inherit pkgs; };
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --nix-path=\\"nixpkgs=${pkgs.path}\\"
          --nix-shell-file nix/shell-stack.nix \
        "
    '';
  };
in with pkgs;

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";
  NIX_PATH = "nixpkgs=${pkgs.path}";
  # CONFIGURE_ARGS = [
  #   "--with-gmp-includes=${gmp.dev}/include"
  #   "--with-gmp-libraries=${gmp.out}/lib"
  #   "--with-curses-includes=${ncurses.dev}/include"
  #   "--with-curses-libraries=${ncurses.out}/lib"
  #   "--with-libnuma-includes=${numactl}/include"
  #   "--with-libnuma-libraries=${numactl}/lib"
  #   "--with-libdw-includes=${elfutils.dev}/include"
  #   "--with-libdw-libraries=${elfutils.out}/lib"
  #   "--enable-dwarf-unwind"
  # ];

  buildInputs = [
    haskell.compiler."ghc${ghcVersion}"
    cabal-install
    stack-wrapped
    nix
    haskell-language-server
    # gmp.dev
    # gmp.out
    # numactl
    # elfutils
    # ncurses.dev
    # ncurses.out
  ];
}
