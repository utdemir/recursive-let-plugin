{ compiler ? "ghc8104" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "recursive-let-plugin" =
        hself.callCabal2nix
          "recursive-let-plugin"
          (gitignore ./.)
          { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."recursive-let-plugin"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };

in
{
  inherit shell;
  inherit myHaskellPackages;
  "recursive-let-plugin" = myHaskellPackages."recursive-let-plugin";
}
