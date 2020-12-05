#!/usr/bin/env bash

ip=$(dig TXT +short o-o.myaddr.l.google.com @ns1.google.com | tr -d '"')
echo ${ip} > /tmp/ip.txt
echo "${ip}" | gh gist create -d ip-$(date --iso-8601).txt
