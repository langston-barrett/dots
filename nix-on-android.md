# Nix on Android

 1. Install GNURoot
 2. Install Debian, log in
 3. `apt-get update`
 4. `apt-get install net-tools dropbear curl`
 5. `passwd`
 6. `ifconfig`
 7. `dropbear -FE -p 2222`
 8. `ssh-copy-id -i ~/.ssh/id_rsa.pub -p 2222 root@<your-ip>`
 9. `dropbear -FE -p 2222 -sg -r /home/root/.ssh/authorized_keys`
 10. `ssh -p 2222 root@<your-ip>`
 11. `apt-get install tmux`
 12. `tmux new`
 13. `curl -O https://github.com/NixOS/nix/archive/1.11.4.tar.gz`
 14. `tar xvf *.tar.gz`
 15. `cd 1*`
 16. `apt-get install make gcc g++ pkg-tools libssl-dev`
 17. `configure`
 18. `make`
 19. `make install`
 20. Done!
