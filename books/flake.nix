{
  description = "File finished book downloads into a library";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      runtimeDeps = [
        pkgs.unzip
        pkgs.libxml2.bin # xmllint
        pkgs.coreutils
        pkgs.diffutils # cmp
        pkgs.exiftool # metadata of PDF, MOBI, AZW3, DJVU
      ];
    in
    {
      packages.${system} = {
        file-books = pkgs.runCommand "file-books" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
          makeWrapper ${pkgs.janet}/bin/janet $out/bin/file-books \
            --add-flags ${./file-books.janet} \
            --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
        '';
        default = self.packages.${system}.file-books;
      };

      # `nix develop`: everything the script needs, to run it straight from here
      devShells.${system}.default = pkgs.mkShell { packages = [ pkgs.janet ] ++ runtimeDeps; };
    };
}
