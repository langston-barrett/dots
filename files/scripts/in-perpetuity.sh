#!/usr/bin/env bash

while true; do
  ${1}
  sleep ${2:-30m}
done
