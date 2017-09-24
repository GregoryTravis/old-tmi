import pprint

def flatten(o):
  if type(o) == str or type(o) == unicode:
    return o
  elif type(o) in [set, list, tuple]:
    return ''.join(map(flatten, o))
  elif type(o) in [int, float]:
    return str(o)
  else:
    assert False, (o, type(o))

def listjoin(os, glue):
  if len(os) < 2:
    return os
  else:
    return [os[0], glue] + listjoin(os[1:], glue)

assert [1, 0, 2, 0, 3] == listjoin([1, 2, 3], 0)
assert [1] == listjoin([1], 0)
assert [] == listjoin([], 0)

assert 'abc' == flatten(['a', ('b',), [['c']]])

def sp(*os):
  for o in os:
    pprint.pprint(o, width=150)
  if len(os) > 0:
    return os[0]

# Convert to string, but with bounded depth.
def bstr(o):
  return pprint.pformat(o, depth=8, width=130000)

trace_indentation = 0
def trace(f):
  def wrapped(*args, **kwargs):
    global trace_indentation
    prefix = '| ' * trace_indentation
    assert len(kwargs) == 0
    print prefix + '+- ' + f.__name__ + '(' + ', '.join(map(str, args)) + ')'
    trace_indentation += 1
    ret = f(*args, **kwargs)
    trace_indentation -= 1
    print prefix + '-> ' + bstr(ret)
    return ret
  wrapped.__name__ = f.__name__
  return wrapped

