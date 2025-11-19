{
  description = "A development shell for corduroy";

  # Flake inputs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; # unstable Nixpkgs

  # Flake outputs
  outputs =
    { self, ... }@inputs:

    let
      # The systems supported for this flake
      supportedSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
        "aarch64-linux" # 64-bit ARM Linux
        "x86_64-darwin" # 64-bit Intel macOS
        "aarch64-darwin" # 64-bit ARM macOS
      ];

      # Helper to provide system-specific attributes
      forEachSupportedSystem =
        f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            pkgs = import inputs.nixpkgs { inherit system; };
          }
        );
    in
    {
      devShells = forEachSupportedSystem (
        { pkgs }:
        {
          default =
            with pkgs;
            mkShell {
              nativeBuildInputs = [
                llvmPackages_18.llvm
                libffi
                libxml2
              ];
              env = {
                LLVM_SYS_181_PREFIX = "${pkgs.llvmPackages_18.llvm.dev}";
              };
            };

        }
      );
    };
}
