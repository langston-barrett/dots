{ pkgs ? import <nixpkgs> { }
, unstable ? import <unstable> { }
}:

pkgs.mkShell {
  # bindgen
  LIBCLANG_PATH = "${unstable.llvmPackages_18.libclang.lib}/lib";
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
    unstable.clang_18
    unstable.llvmPackages_18.llvm.dev
    pkgs.qemu_kvm
    pkgs.ninja
    pkgs.libslirp
    pkgs.pkg-config

    pkgs.linuxKernel.packages.linux_5_10.perf
    pkgs.mdbook
  ];
}
