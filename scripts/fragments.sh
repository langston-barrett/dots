#!/usr/bin/env bash

set -exuo pipefail

cd kludge
cargo install --path=.
wd=${PWD}
for dir in lesshand marki mdlynx tree{-crasher,-splicer,reduce}; do
  cd ~/code/"${dir}"
  git checkout main
  git pull origin main
  git branch -D bump-linters || true
  git checkout -b bump-linters
  kludge fragments "${wd}/skel" "${wd}/fragments"
  git add .
  if git commit -m 'chore: Update linters, actions'; then
    gh pr create --assignee langston-barrett
    gh pr merge --auto --rebase
  else
    git checkout main
    git stash
    git stash drop
  fi
done
