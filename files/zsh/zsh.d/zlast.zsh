#!/usr/bin/env zsh

# this file is sourced last

# GREASE

my-expand-abbrev() {
  typeset -A abbrevs
  if [[ $PWD == ~/code/detect ]]; then
    abbrevs=(
      "clippy" "echo 'cargo clippy --all-targets -- --deny warnings'"
      "bs" "echo 'echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid && sudo sysctl kernel.perf_event_mlock_kb=2048 && cargo b -q --profile=profiling --bin=sofuzz && samply record ./target/profiling/sofuzz --solutions /run/user/1000/sols --gas=2048 sofuzz/rs/map/map.toml target/profiling/libsofuzz_map.so --no-check-dwarf'"
      "e1" "echo 'rm -rf benign solutions ; cargo build -p=eval1-smi-model && cargo run --bin dxezz -- --qcow=targets/eval1-smi/image-debug/snapshots.qcow2 targets/eval1-smi/eval1-smi-debug.toml target/debug/libeval1_smi_model.so --seed=1 --outer-iterations=8 --inner-iterations=1 --no-check-snapshots -v'"
      "rd" "echo 'cargo run --bin=dxezz --'"
      "rs" "echo 'cargo run --bin=sofuzz --'"
      "t" "echo 'cargo test'"
      "td" "echo 'cargo test --package=dxezz -- --test-threads=1'"
      "ts" "echo 'cargo b -q --package=sofuzz-boxcar && cargo b -q --package=sofuzz-map && cargo test --package=sofuzz'"
      "x" "export NO_REBUILD_QEMU_SYS=1"
    )
  elif [[ $PWD == ~/code/grease ]]; then
    abbrevs=(
      "r" "echo 'cabal run exe:grease'"
      "t" "echo 'cabal run test:grease-tests --'"
      "to" 'echo "cabal run exe:grease -- --symbol test $(fd --type=x elf tests/ | zshfzf)"'
    )
  fi

  if [[ $BUFFER == "help" ]]; then
    help=""
    for key value in ${(kv)abbrevs}; do
      help="$help $key"
    done
    zle -M "${(e)help}"
    zle -R
    sleep 5
    return
  fi

  for key value in ${(kv)abbrevs}; do
    check=${BUFFER/#$key/}
    if [[ $BUFFER != $check ]] && [[ ${#BUFFER} == ${#key} ]]; then
      new=$(eval "$value")
      BUFFER="${BUFFER/#$key/$new} "
      zle -R
      CURSOR=${#BUFFER}
      return
    fi
  done
  zle zbr-space
}

zle -N my-expand-abbrev
bindkey " " my-expand-abbrev
