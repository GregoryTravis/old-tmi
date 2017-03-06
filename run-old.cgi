#!/usr/local/bin/python

# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

import traceback

try:
  import old
  import web

  web.webmain(old)
except:
  print 'Content-type: text/html\n'
  print '<pre>' + traceback.format_exc()
