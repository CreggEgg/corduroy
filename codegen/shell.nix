# https://nix.dev/tutorials/declarative-and-reproducible-developer-environments
with import <nixpkgs> { };

mkShell {

  # Package names can be found via https://search.nixos.org/packages
  nativeBuildInputs = [
    direnv
    llvmPackages_18.llvm
    libffi
    libxml2
  ];

  LLVM_SYS_181_PREFIX = "${pkgs.llvmPackages_18.llvm.dev}";

  NIX_ENFORCE_PURITY = true;

  shellHook = '''';
}
