import json
import os
import sys
import traceback
import urllib

DEFAULT_QUERY_STRING = json.dumps(('main', []))

def getQueryString():
  if 'QUERY_STRING' in os.environ:
    return os.environ['QUERY_STRING']
  elif len(sys.argv) > 1:
    assert len(sys.argv) == 2
    return sys.argv[1]
  else:
    return ''

def getScript():
  script = sys.argv[0]
  if script.startswith('./'):
    script = script[2:]
  if 'CONTEXT_DOCUMENT_ROOT' in os.environ and script.startswith(os.environ['CONTEXT_DOCUMENT_ROOT']):
    script = script[len(os.environ['CONTEXT_DOCUMENT_ROOT']):]
  return script

def link(url, text):
  return '<a href="%s">%s</a>' % (url, text)

def call(text, f, *args, **kwargs):
  assert len(kwargs) == 0
  url = getScript() + '?' + urllib.quote(json.dumps((f.__name__, args)))
  return link(url, text)

def exec_call(module, s):
  (funname, args) = json.loads(urllib.unquote(s))
  fun = module.__dict__[funname]
  return fun(*args)
  print funs
  print module.main

def pre(s):
  return '<pre>%s</pre>' % s

def webfmt(s):
  return pre(s.encode('utf-8'))

def webmain(module):
  print 'Content-type: text/html\n'
  try:
    qs = getQueryString()
    if qs == '':
      qs = DEFAULT_QUERY_STRING
    q = exec_call(module, qs)
    print webfmt(exec_call(module, qs))
  except Exception as e:
    print pre(traceback.format_exc())

