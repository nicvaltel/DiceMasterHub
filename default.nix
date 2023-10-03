{ nixpkgs ? import <nixpkgs> {  } }:
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "ea5234e7073d5f44728c499192544a84244bf35a";
  nixpkgsSha = "sha256:1iqfglh1fdgqxm7n4763k1cipna68sa0cb3azm2gdzhr374avcvk";
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      pkgs.emacs
      pkgs.vscode
      pkgs.tree # show files in directory as tree
      pkgs.mc
      pkgs.postgresql
      pkgs.dbeaver
      pkgs.glibc
      pkgs.docker
      pkgs.docker-compose
      pkgs.websocat # Websocat is a command-line utility that can help you test WebSocket connections

      compiler.stack
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      compiler.ghcide

      pkgs.ormolu
      pkgs.hpack
      compiler.ghc
      pkgs.haskellPackages.record-dot-preprocessor
      pkgs.zlib
      pkgs.pcre # A library for Perl Compatible Regular Expressions

      pkgs.go
      pkgs.gopls # Official language server for the Go language
      
      # # pkgs.haskellPackages.postgresql-simple-migration

    ];


#     virtualisation.docker.enable = true;
#     users.users.kolay = {
#       isNormalUser = true;
#       home = "/home/kolay";
#       extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" "audio"]; # "wheel" Enable ‘sudo’ for the user.
#     };

     shellHook = ''
         echo "Entering my Nix shell environment..."
         cd DB
         konsole -e 'docker-compose up' &
         cd ..
         konsole &
         konsole &
         dbeaver &
         code . &

         #!/usr/bin/env ./run_code.sh
         # You can add any additional setup or configuration here
         # For example, you can run your start.sh script:
         # $myEnv/bin/bash $start.sh
     '';

}


# To run HLS:
# stack new MyProgram
# rm Setup.hs 
# rm stack.yaml 
# rm MyProgram.cabal
# rm -rf .stack-work/
# hpack
# gen-hie > hie.yaml
# cabal build
