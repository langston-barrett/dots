#!/usr/bin/env python
# -*- mode: python -*-

# def passwd():
#   with open('/home/siddharthist/.mutt/password') as f:
#     return f.read()

from subprocess import check_output

def passwd():
    return check_output("gpg -dq ~/.mutt/offlineimappass.gpg", shell=True).strip("\n")
