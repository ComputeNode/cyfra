{
  description = "Dev shell for Vulkan + Java 21";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.jdk21;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            jdk
            sbt
            vulkan-tools
            vulkan-loader
            vulkan-validation-layers
            glslang
            pkg-config
          ];

          JAVA_HOME = jdk;
          VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
          LD_LIBRARY_PATH="${pkgs.vulkan-loader}/lib:${pkgs.vulkan-validation-layers}/lib:$LD_LIBRARY_PATH";

        };
      });
}

