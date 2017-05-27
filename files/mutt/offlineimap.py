#!/usr/bin/env python
# -*- mode: text -*-

def passwd():
  with open('/home/siddharthist/.mutt/password') as f:
    return f.read()
