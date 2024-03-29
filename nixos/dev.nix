{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    clang
    docker-compose
    patchutils
    llvm
    rr
    shellcheck

    # Python development

    # python3Packages.importmagic
    # TODO: Set up default config file
    # python3Packages.pylint
    # python3Packages.python-language-server
  ];

  # TODO: Fix after NixOS 21.05 upgrade
  # security.apparmor.policies =
  #   let writeDenyProfile =
  #         import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
  #   in {
  #     shellcheck =
  #       writeDenyProfile { path = pkgs.shellcheck; binary = "shellcheck"; }
  #   };
}
