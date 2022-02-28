{
  description="Logical Framework based on the λΠ-calculus modulo";
  inputs = {
    nixpkgs.url = "nixpkgs";
  };
  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux = import ./shell.nix { pkgs = import nixpkgs { system = "x86_64-linux"; }; };
  };
}
