# Nix on Android

## Set up SSH
 1. Install GNURoot
 2. Install Debian, log in
 3. `passwd`
 4. `apt-get update`
 5. `apt-get -y install dropbear curl`
 6. `curl icanhazip.com`
 7. `dropbear -FE -p 2222`

## Secure SSH
 1. `ssh-copy-id -i ~/.ssh/id_rsa -p 2222 root@<your-ip>`
 2. `ssh -p 2222 -i ~/.ssh/id_rsa root@<your-ip> `
 3. `cat ~/.ssh/authorized_keys >> /etc/dropbear/authorized_keys`
 4. `nohup dropbear -p 2223 -FE -sg &`
 5. `kill <old-dropbear-pid> && exit`
 6. `ssh -p 2223 -i ~/.ssh/id_rsa -o PreferredAuthentications=publickey -t root@<your-ip>`
   - Add this host to your ~/.ssh/config

## tmux
 1. `apt-get -y install tmux`
 2. `tmux new`
 3. If you get disconnected, `ssh -p 2223 -i ~/.ssh/id_rsa -o PreferredAuthentications=publickey -t root@<your-ip> tmux attach`

## Compile
 3. `curl -O https://github.com/NixOS/nix/archive/1.11.4.tar.gz`
 4. `tar xvf *.tar.gz`
 5. `cd 1*`
 6. `apt-get install make gcc g++ pkg-tools libssl-dev` etc.
 7. `configure`
   - Install missing software, libraries
 8. `make`
 9. `make install`
 10. Done!
