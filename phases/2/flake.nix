{
  inputs = { utils.url = "github:numtide/flake-utils"; };
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        rEnv = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            languageserver
            lintr
            here
            DoE_base
            FrF2
            tidyverse
            janitor
          ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [ rEnv pkgs.texlive.combined.scheme-full ];
        };
      });
}
