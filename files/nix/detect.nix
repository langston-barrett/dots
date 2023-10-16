{ pkgs ? import <nixpkgs> { }
, unstable ? import <unstable> { }
}:

pkgs.mkShell {
  # bindgen
  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
  QEMU = "${pkgs.qemu_kvm}";

  buildInputs = [
    # libafl_qemu
    pkgs.glib
    pkgs.pixman
  ];

  nativeBuildInputs = [
    pkgs.rust-analyzer
    pkgs.rustup

    # libafl_qemu
    unstable.clang_16
    pkgs.qemu_kvm
    pkgs.ninja
    pkgs.libslirp
    pkgs.pkg-config

    pkgs.linuxKernel.packages.linux_5_10.perf
    pkgs.mdbook
  ];
}
