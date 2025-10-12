{
  description = "cl-task-runner flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        systemInputs = with pkgs; [pkg-config
                                   openssl
                                   libev
                                   postgresql_16_jit
                                   pgformatter
                                   pgcli
                                  ];
        lispInputs = (with pkgs; [sbcl]) ++ (with pkgs.sbclPackages; [swank]);
      in {
        devShell = pkgs.mkShell {
          name = "cl-task-runner";
          buildInputs = systemInputs ++ lispInputs;
          shellHook = ''
 export EDITOR=emacs;
 export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath[
   pkgs.openssl
   pkgs.libev
   pkgs.postgresql_16_jit
 ]};
 '';
        };
      });
}
