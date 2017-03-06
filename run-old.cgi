#!/usr/local/bin/python

# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

import httpwrap

with httpwrap.HttpWrap():
  import tmi
  import old
  import web
  web.webmain(old)
