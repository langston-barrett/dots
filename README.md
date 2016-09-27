# dots
[![Build Status](https://travis-ci.org/siddharthist/dots.svg?branch=master)](https://travis-ci.org/siddharthist/dots)

These are not your everyday dotfiles! This repo hosts both per-system
configuration via NixOS and per-user configuration via Ansible.

Ansible roles aren't used to enforce minimal complexity. Ansible is only used
for symlinking and mild templating.

Anything complex or involving services/packages is upstreamed to nixpkgs.

TODO:
 - [ ] dunst
 - [ ] gpg + agent
 - [ ] photo management via lychee
 - [ ] secure/ for ssh, gpg keys, network passwords
 - [ ] acpid - handle power events
 - When it hits 1.0 - https://github.com/shockone/black-screen
 - https://github.com/tldr-pages/tldr
