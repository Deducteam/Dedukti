{ pkgs ? import <nixpkgs> {}, ... }:

let
  opam = "${pkgs.opam}/bin/opam";
in pkgs.mkShell {
  # Packages to install
  packages = builtins.attrValues {
    inherit (pkgs)
      opam
      dune_2
      dune-release
      python27Full
      ;
  };

  shellHook = ''
    eval $(${opam} env --switch=dedukti)
  '';

  CFLAGS="-I${pkgs.gmp.dev}/include -I${pkgs.mpir}/include -L${pkgs.gmp.dev}/lib -L${pkgs.mpir}/lib";
  LDFLAGS="-L${pkgs.gmp.dev}/lib -L${pkgs.mpir}/lib";
  LD_LIBRARY_PATH="${pkgs.gmp.dev}/lib:${pkgs.mpir}/lib";
}
