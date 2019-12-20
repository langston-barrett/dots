#!/usr/bin/env bash

set -e

# sudo passwd siddharthist pass
# sudo passwd vagrant pass
rm -rf dots/ || true
git clone --branch master --depth 1 https://github.com/langston-barrett/dots
cd dots/
# TODO: debug this
HOME=/home/siddharthist bash run.sh || true
