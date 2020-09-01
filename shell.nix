let srcs = import nix/sources.nix;
    thoralf-overlay =
      self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            # ghc = super.haskell.compiler.ghc882;
            thoralf-plugin =
              super.haskell.lib.dontCheck
              (hself.callCabal2nix "thoralf-plugin"
                srcs.the-thoralf-plugin {});
            };
          };
      };

    pkgs = (import srcs.nixpkgs {
      overlays = [ thoralf-overlay ];
    });
in
with pkgs;
mkShell {
  name = "dumb-shell";
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p;
      [ thoralf-plugin
        first-class-families
        vector
      ]))
    haskellPackages.cabal-install
    z3
  ];
}
