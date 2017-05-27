{ stdenv, pythonPackages, fetchFromGitHub }:
# { pkgs ? import <nixpkgs> { } }:

pythonPackages.buildPythonPackage rec {
  name = "elib_intl-${version}";
  version = "2011-07";

  src = fetchFromGitHub {
    owner = "dieterv";
    repo = "elib.intl";
    rev = "d09997cfef8584f9f0bf227752bab890a66a0a61";
    sha256 = "0y7vzff9xgbnaay7m0va1arl6g68ncwrvbgwl7jqlclsahzzb09d";
  };

  propagatedBuildInputs = [ pythonPackages.nose ];

  meta = with stdenv.lib; {
    description = "The elib.intl module provides enhanced internationalization (I18N) services for your Python modules and applications.";
    homepage = https://github.com/dieterv/elib.intl/;
    maintainers = [ maintainers.siddharthist ];
    license = licenses.mit;
  };
}
