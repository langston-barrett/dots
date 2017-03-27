# Nix on Android

Some instructions copied faithfully from https://nixos.org/wiki/Bootstrapping_NixOS_on_ARM.

## Set up SSH
 1. Install GNURoot
 2. Install Debian, log in
 3. `passwd`
 4. `apt-get update`
 5. `apt-get -y install curl dropbear`
 6. `curl icanhazip.com`
 7. `dropbear -FE -p 2222`

## Secure SSH
 1. `ssh-copy-id -i ~/.ssh/id_rsa -p 2222 root@<your-ip>`
 2. `ssh -p 2222 -i ~/.ssh/id_rsa root@<your-ip> `
 3. Dropbear: `cat ~/.ssh/authorized_keys >> /etc/dropbear/authorized_keys`
 4. No passwords, but root login: `nohup dropbear -p 2223 -FE -sg &`
 5. `kill <old-dropbear-pid> && exit`
 6. `ssh -p 2223 -i ~/.ssh/id_rsa -o PreferredAuthentications=publickey -t root@<your-ip>`
   - Add this host to your ~/.ssh/config

## tmux
 1. `apt-get -y install tmux`
 2. `tmux new`
 3. If you get disconnected, `ssh -p 2223 -i ~/.ssh/id_rsa -o PreferredAuthentications=publickey -t root@<your-ip> tmux attach`

## Compile
 1. `curl -O https://github.com/NixOS/nix/archive/1.11.4.tar.gz`
 2. `tar xvf *.tar.gz`
 3. `cd 1*`
 4. `apt-get install make gcc g++ pkg-tools libssl-dev` etc.
 5. `configure`
   - Install missing software, libraries
 6. `make`
 7. `make install`
 8. Done!

## Nix
```bash
apt-get install libbz2-dev libsqlite3-dev libcurl4-openssl-dev \
  libdbd-sqlite3-perl libwww-curl-perl
groupadd -r nixbld
for n in $(seq 1 10); do useradd -c "Nix build user $n" \
    -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(which nologin)" \
            nixbld$n; done
git clone --branch master --depth 1 https://github.com/NixOS/nixpkgs.git nixpkgs
echo 'export NIX_PATH=$HOME' >> /etc/profile
echo 'export NIX_PATH=$HOME' >> ~/.profile
source /etc/profile
```


## Ansible
 - hostname
 - PATH=$PATH:/usr/sbin
