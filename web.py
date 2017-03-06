import cgi
import Cookie
import json
import os
import sys
import traceback
import urllib
from tags import *
from tmi import *

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
  elif type(o) in [set, list, tuple]:
    return ''.join(map(flatten, o))
  else:
    assert False, (o, type(o))

assert 'abc' == flatten(['a', ('b',), [['c']]])

def webfmt(s):
  return flatten(s).encode('utf-8')

COOKIE_PREFIX = 'tmi'
_cookies = {}

def readCookies():
  global _cookies
  if 'HTTP_COOKIE' not in os.environ: return
  c = Cookie.SimpleCookie()
  c.load(os.environ['HTTP_COOKIE'])
  _cookies = {k[len(COOKIE_PREFIX):]: v.value for k, v in c.iteritems() if k.startswith(COOKIE_PREFIX)}

@node
class Cookies(Node):
  def forwards():
    return _cookies
  def backwards(out):
    global _cookies
    _cookies = out
    return {}

def generateCookieHeader():
  return '\n'.join(['Set-Cookie: %s%s=%s' % (COOKIE_PREFIX, k, urllib.quote(v)) for k, v in _cookies.iteritems()])

def webmain(module):
  readCookies()
  request_method = os.environ['REQUEST_METHOD']
  result = None
  #print 'Content-type: text/html\n'
  if request_method == 'GET':
    qs = getQueryString()
    if qs == '':
      qs = DEFAULT_QUERY_STRING
    result = exec_call(module, qs)
  elif request_method == 'POST':
    field_storage = cgi.FieldStorage()
    rec = {k: field_storage[k].value for k in field_storage if k != '_destfun'}
    result = module.__dict__[field_storage['_destfun'].value](rec)
  else:
    assert False

  commit()

  print 'Content-type: text/html'
  print generateCookieHeader()
  print ''
  print webfmt(read(result))

def listjoin(os, glue):
  if len(os) < 2:
    return os
  else:
    return [os[0], glue] + listjoin(os[1:], glue)

assert [1, 0, 2, 0, 3] == listjoin([1, 2, 3], 0)
assert [1] == listjoin([1], 0)
assert [] == listjoin([], 0)

def mkform(destfun, rec):
  return ('Login:', br(), form(
    hidden('_destfun', destfun.__name__),
    listjoin([[k, input(name=k, value=v)] for k, v in rec.iteritems()], br()),
    br(),
    button('Submit'),
    method='POST',
    action='/' + getScript() + '?'))
