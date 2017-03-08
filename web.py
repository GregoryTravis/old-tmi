import cgi
import Cookie
import io
import json
import os
import shutil
import sys
import traceback
import urllib
from tags import *
from tmi import *

DEFAULT_QUERY_STRING = json.dumps(('main', []))

def getScript():
  script = sys.argv[0]
  if script.startswith('./'):
    script = script[2:]
  if 'CONTEXT_DOCUMENT_ROOT' in os.environ and script.startswith(os.environ['CONTEXT_DOCUMENT_ROOT']):
    script = script[len(os.environ['CONTEXT_DOCUMENT_ROOT'])+1:]
  return script

def ahref(text, url):
  return '<a href="%s">%s</a>' % (url, text)

def call(f, *args, **kwargs):
  assert len(kwargs) == 0
  args = map(readIfNode, args)
  return '/' + getScript() + '?' + urllib.quote(json.dumps((f.__name__, args)))

def link(text, f, *args, **kwargs):
  return ahref(text, call(f, *args, **kwargs))

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
  elif type(o) in [int, float]:
    return str(o)
  else:
    assert False, (o, type(o))

assert 'abc' == flatten(['a', ('b',), [['c']]])

COOKIE_PREFIX = 'tmi'
_cookies = {}

def readCookies(http_cookie):
  global _cookies
  if http_cookie == None:
    return
  c = Cookie.SimpleCookie()
  c.load(http_cookie)
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

def get_inputs():
  field_storage = cgi.FieldStorage()
  form_data = {k: field_storage[k].value for k in field_storage}
  return {
    'HTTP_COOKIE': os.environ.get('HTTP_COOKIE', None),
    'REQUEST_METHOD': os.environ['REQUEST_METHOD'],
    'QUERY_STRING': os.environ['QUERY_STRING'],
    'form_data': form_data,
  }

def record(inputs, output):
  isRecording = os.path.exists('recording')
  if not isRecording:
    return
  # Shouldn't hard-code Old here.
  if not os.path.exists('recording/out'):
    os.mkdir('recording/out')
  db = read(File('old.dat'))
  if os.path.exists('recording/counter'):
    with open('recording/counter') as f:
      counter = int(f.read())
  else:
    counter = 0
    # Also the initial copy
    shutil.copyfile('old.dat', 'recording/initial.db')
  with open('recording/counter', 'w') as f:
    f.write(str(counter + 1))
  with open('recording/%s.input' % counter, 'w') as f:
    f.write(str(inputs))
  with open('recording/out/%s.output' % counter, 'w') as f:
    f.write(str(output))
  with open('recording/out/%s.db' % counter, 'w') as f:
    f.write(str(db))

def webmain(module):
  inputs = get_inputs()
  result = run_things(module, inputs)
  assert result != None
  commit()
  output = format_result(result)
  record(inputs, output)
  sys.stdout.write(output)

def webtestmain(module, recording):
  # TODO hack
  sys.argv[0] = 'run-old.cgi'
  counter = 0
  shutil.copyfile('%s/initial.db' % recording, 'old.dat')
  while os.path.exists('%s/%s.input' % (recording, counter)):
    inputs = io.readdat('%s/%s.input' % (recording, counter))

    result = run_things(module, inputs)
    assert result != None
    commit()
    output = format_result(result)
    record(inputs, output)

    counter += 1
  print '-', recording, counter

def run_things(module, inputs):
  readCookies(inputs['HTTP_COOKIE'])
  request_method = inputs['REQUEST_METHOD']
  result = None
  if request_method == 'GET':
    qs = inputs['QUERY_STRING']
    if qs == '':
      qs = DEFAULT_QUERY_STRING
    return exec_call(module, qs)
  elif request_method == 'POST':
    # Should remove _destfun from the formdata
    fd = inputs['form_data']
    return module.__dict__[fd['_destfun']](fd)
  else:
    assert False

def format_result(result):
  # TODO this is hacky.
  if type(result) == dict and result.keys() == ['redirect']:
    return (
      generateCookieHeader() + '\n' +
      'Location: ' + result['redirect'] + '\n' +
      # Necessary to force an external redirect which is necessary for cookies to
      # work across the redirect.
      'Status: 302\n\n')
  else:
    return (
      'Content-type: text/html\n' +
      generateCookieHeader() + '\n\n' +
      flatten(readIfNode(result)).encode('utf-8'))

def redirect(f, *args):
  return {'redirect': call(f, *args)}

def listjoin(os, glue):
  if len(os) < 2:
    return os
  else:
    return [os[0], glue] + listjoin(os[1:], glue)

assert [1, 0, 2, 0, 3] == listjoin([1, 2, 3], 0)
assert [1] == listjoin([1], 0)
assert [] == listjoin([], 0)

def mkform(destfun, rec, hidden_rec={}):
  return form(
    hidden('_destfun', destfun.__name__),
    listjoin([[k, input(name=k, value=v)] for k, v in rec.iteritems()], br()),
    [hidden(k, v) for k, v in hidden_rec.iteritems()],
    br(),
    button('Submit'),
    method='POST',
    action='/' + getScript() + '?')

@node
class ListJoin(UNode):
  def forwards(os, glue):
    return listjoin(list(os), glue)
