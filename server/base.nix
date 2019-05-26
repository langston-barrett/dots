{ pkgs ? import <nixpkgs> { }
, name
, pkg ? pkgs.${name}
, opts
}:

# http://lethalman.blogspot.com/2016/04/cheap-docker-images-with-nix_15.html
# docker load < result
# sudo docker run --rm --net=host --volume="./syncthing/:/data:rw"

with pkgs; with dockerTools;
buildImage {
  inherit name;
  tag = "latest";

  contents = pkg;
  runAsRoot = ''
    #!${stdenv.shell}
    ${dockerTools.shadowSetup}
    groupadd -r ${name}
    useradd -r -g ${name} -d /data -M ${name}
    mkdir /data
    chown ${name}:${name} /data
  '';

  config = {
    Cmd = [
      "${gosu.bin}/bin/gosu"
      "${name}"
      "${pkg}/bin/${name}"
    ] ++ opts;
    WorkingDir = "/data";
    Volumes = {
      "/data" = {};
    };
  };
}
