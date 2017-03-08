#!/usr/local/bin/python

# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

test = sys.argv[1]

import old
import web
web.webtestmain(old, test)
