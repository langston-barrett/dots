# { stdenv, fetchFromGitHub, python2Packages, intltool }:
{ pkgs ? import <nixpkgs> { } }:

let
  # Pin a nixpkgs version
  pinned_pkgs = import (pkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "17.03";
    sha256 = "1fw9ryrz1qzbaxnjqqf91yxk1pb9hgci0z0pzw53f675almmv9q2";
  }) {};

  elib_intl =
    with pinned_pkgs; callPackage ./elib_intl.nix {
      stdenv = stdenv;
      pythonPackages = pythonPackages;
      fetchFromGitHub = fetchFromGitHub;
    };
in with pinned_pkgs; python2Packages.buildPythonApplication rec {
  name = "gourmet-${version}";
  version = "0.17.4";

  src = fetchFromGitHub {
    owner  = "thinkle";
    repo   = "gourmet";
    rev    = "c0df1579e6d4ea01be037fd6b75aa15643bc6a00";
    sha256 = "09a2zk140l4babwdj8pwcgl9v7rvwff9cn7h3ppfhm3yvsgkrx07";
  };

  # https://github.com/thinkle/gourmet/blob/master/INSTALL.md
  nativeBuildInputs = [
    python2Packages.distutils_extra
    intltool
  ];

  # Optional requirements for plugins: pygst, gtkspell, beautifulsoup, ipython
  propagatedBuildInputs = with python2Packages; [
    pygtk
    pygobject3
    sqlalchemy
    pillow
    elib_intl
  ];

  postInstall = ''
    mkdir -p $out/gourmet
    cp -r data $out/gourmet/
    cp -r ui $out/gourmet/
  '';

  meta = with stdenv.lib; {
    homepage = https://thinkle.github.io/gourmet/;
    description = "Gourmet Recipe Manager";
    longDescription = ''
      Gourmet Recipe Manager is a manager, editor, and organizer for recipes. It
      has a plugin architecture which allows you to enable extensions to
      Gourmet's base functionality. For example, there is a nutritional plugin
      that allows Gourmet to help you calculate nutritional information for any
      recipe. There are also a wide variety of import and export plugins that
      let Gourmet read and write recipes in various formats.
    '';
    license = stdenv.lib.licenses.gpl2;
    platforms = stdenv.lib.platforms.linux;
  };
}
