{ pkgs ? import <nixpkgs> {},
  compiler ? "ghc8104"
}:
let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  haskellLib = pkgs.haskell.lib;

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = se: su: {
      "nixos-manager" = haskellLib.overrideCabal
        (se.callCabal2nix "nixos-manager" (gitignore ./.) {})
        (drv: {
          buildTools = drv.buildTools or [] ++ [ pkgs.makeWrapper ];
          postFixup = ''
            wrapProgram $out/bin/nixos-manager \
            --prefix PATH : "${pkgs.lib.makeBinPath [pkgs.gksu]}"
          '';
        });
    };
  };

in
rec
{
  "nixos-manager" = myHaskellPackages."nixos-manager";
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."nixos-manager"
    ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      hlint
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };
}
