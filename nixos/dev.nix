{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    entr

    # For Emacs markdown-preview-eww
    ruby
    rubyPackages.redcarpet

    clang
    docker-compose
    llvm
    rr
    shellcheck

    # Python development

    # python3Packages.importmagic
    # TODO: Set up default config file
    # python3Packages.pylint
    python3Packages.python-language-server
  ];

  security.apparmor.profiles =
    let writeDenyProfile =
          import ./functions/apparmor-deny-profile.nix { inherit pkgs; };
    in [
      (writeDenyProfile { path = pkgs.shellcheck; binary = "shellcheck"; })
    ];
}
