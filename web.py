import cgi
import json
import os
import sys
import traceback
import urllib
from tags import *

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
    script = script[len(os.environ['CONTEXT_DOCUMENT_ROOT'])+1:]
  return script

def link(url, text):
  return '<a href="%s">%s</a>' % (url, text)

def call(text, f, *args, **kwargs):
  assert len(kwargs) == 0
  url = '/' + getScript() + '?' + urllib.quote(json.dumps((f.__name__, args)))
  return link(url, text)

def exec_call(module, s):
  (funname, args) = json.loads(urllib.unquote(s))
  fun = module.__dict__[funname]
  return fun(*args)

def pre(s):
  return '<pre>%s</pre>' % s

def flatten(o):
  if type(o) == str or type(o) == unicode:
    return o
  else:
    return ''.join(map(flatten, o))

assert 'abc' == flatten(['a', ('b',), [['c']]])

def webfmt(s):
  return flatten(s).encode('utf-8')

def webmain(module):
  print 'Content-type: text/html\n'
  request_method = os.environ['REQUEST_METHOD']
  try:
    if request_method == 'GET':
      qs = getQueryString()
      if qs == '':
        qs = DEFAULT_QUERY_STRING
      q = exec_call(module, qs)
      print webfmt(exec_call(module, qs))
    elif request_method == 'POST':
      field_storage = cgi.FieldStorage()
      rec = {k: field_storage[k].value for k in field_storage if k != '_destfun'}
      print module.__dict__[field_storage['_destfun'].value](rec)
    else:
      assert False
  except Exception as e:
    print pre(traceback.format_exc())

def listjoin(os, glue):
  if len(os) < 2:
    return os
  else:
    return [os[0], glue] + listjoin(os[1:], glue)

assert [1, 0, 2, 0, 3] == listjoin([1, 2, 3], 0)
assert [1] == listjoin([1], 0)
assert [] == listjoin([], 0)

def mkform(destfun, rec):
  return form(
    hidden('_destfun', destfun.__name__),
    listjoin([input(name=k, value=v) for k, v in rec.iteritems()], br()),
    br(),
    button('Submit'),
    method='POST',
    action='/' + getScript() + '?')
