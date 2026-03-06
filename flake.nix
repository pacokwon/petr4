{
  description = "Flake for Petr4 harness";

  inputs = {
    # Pinning to the release-22.05 branch
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }: 
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ 
          pkgs.opam
          pkgs.m4
          pkgs.gmp
          pkgs.pkg-config
        ];

        shellHook = ''
          # Initialize opam if it hasn't been already
          export OPAMROOT="$PWD/.opam"
          if [ ! -d "$OPAMROOT" ]; then
            opam init --bare --disable-sandboxing -n
          fi

          # Load opam environment
          eval $(opam env)
        '';
      };
    };
}

