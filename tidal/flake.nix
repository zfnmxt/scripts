{
  description = "Mirror a Tidal library into a music folder with tiddl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    packages = {
      url = "git+ssh://git@github.com/zfnmxt/packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      packages,
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (packages.packages.${system}) tiddl;
      runtimeDeps = [
        pkgs.janet
        pkgs.curl
        pkgs.jq
        pkgs.gnused
        tiddl
      ];
    in
    {
      packages.${system} = {
        inherit tiddl;
        tidal-library = pkgs.runCommand "tidal-library" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
          makeWrapper ${pkgs.janet}/bin/janet $out/bin/tidal-library \
            --add-flags ${./tidal-library.janet} \
            --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
        '';
        default = self.packages.${system}.tidal-library;
      };

      # `nix develop`: everything the script needs, to run it straight from here
      devShells.${system}.default = pkgs.mkShell { packages = runtimeDeps; };
    };
}
