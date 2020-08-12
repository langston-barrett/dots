{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    clang
    docker-compose
    llvm
    rr
    shellcheck
  ];

  security.apparmor.profiles =
    let writeDenyProfile =
          import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
    in [
      (writeDenyProfile { path = pkgs.shellcheck; binary = "shellcheck"; })
    ];
}
