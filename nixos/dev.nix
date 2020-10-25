{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    entr

    # For Emacs markdown-preview-eww
    ruby
    rubyPackages.redcarpet

    python37Packages.python-language-server
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
