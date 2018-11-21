#!/usr/bin/env zsh

# Delete all files with the given extensions (list as string, separated by " ")
function remove_exts() {
  setopt shwordsplit # Split on " "
  # Group with or: Start with \(, Separate exts with \|, end with \)
  re='^.+\.\('; for ext in $1; do re+=$ext && re+='\|'; done; re+='\)$'
  # Now, find and delete based on that regexp
  find . -iregex "$re" -exec rm {} +
  unsetopt shwordsplit
}
function tobak() { mv "$1"{,.bak} ; }

# Description: move up this number of directories
# Arguments: $1 - number of directories to go up
# Returns (echoes): none
function up() {
  # Description: repeat a string $1 times
  # Arguments: $1 - str, $2 - Number of times to repeat the string
  function repeat_string() {
    if [ -z "$2" ] || [ "$2" -eq 0 ];
    then echo ""
    else echo $1$(repeat_string "$1" $(expr $2 - 1))
    fi
  }
  [ -z "$1" ] && 1="1"
  cd $(repeat_string "../" $1)
}

# mount encrypted disks
mount_encrypted() {
  labels=$(ls --color=never /dev/disk/by-label)
  sudo cryptsetup luksOpen $1 encrypted
  sudo mkdir -p /mnt/$labels
  sudo mount /dev/mapper/encrypted /mnt/$labels
}

# Convert all flac files in a directory to ogg
to_ogg() {
  array=()
  while IFS=  read -r -d $'\0'; do array+=("$REPLY");
    done < <(find . -name "*.flac" -print0)
    # array now holds the names of all flac files
  for flac in $array; do oggenc -q 7 "$flac" && rm -f "$flac"; done
}

# Recursively find and replace
sed_recurse() { ag -g '.*' -0 | xargs -0 sed -i "$1"; }

# https://bit.ly/2ydBgfQ
in_interactive_session() {
  [[ $- == *i* ]]
}

# https://github.com/andreafrancia/trash-cli
alias tp='trash-put'
alias tl='trash-list'
alias rm='echo Use \\ rm or tp \(trash-put\)'

copy_last_command() {
  echo "!!" | xsel -ib
}
