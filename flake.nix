{
    outputs = { self, nixpkgs }: let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskell.packages.ghc924;
      in
    {
        devShells.x86_64-linux.default = pkgs.mkShell
        {
            buildInputs = with pkgs; [
                libsodium
                secp256k1
                pkg-config
            ] ++ (with hpkgs; [ ghc cabal-install ]);

        };
    };
}
