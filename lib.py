import atexit
import collections
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
# Custom printer only looks at args, not kwargs
def ctrace(preproc):
  def decorator(f):
    def wrapped(*args, **kwargs):
      argsrep = ', '.join(map(str, (args if preproc == None else preproc(args))))
      global trace_indentation
      prefix = '| ' * trace_indentation
      assert len(kwargs) == 0
      print prefix + '+- ' + f.__name__ + '(' + argsrep + ')'
      trace_indentation += 1
      ret = f(*args, **kwargs)
      trace_indentation -= 1
      print prefix + '-> ' + bstr(ret)
      return ret
    wrapped.__name__ = f.__name__
    return wrapped
  return decorator

def trace(f):
  return ctrace(None)(f)

def listsplit(os, p):
  if len(os) == 0:
    return []
  oses = [[]]
  for o in os:
    if p(o):
      oses.append([])
    else:
      oses[-1].append(o)
  return oses

assert [] == listsplit([], lambda x: x == 0)
assert [[1]] == listsplit([1], lambda x: x == 0)
assert [[1], []] == listsplit([1, 0], lambda x: x == 0)
assert [[1, 1]] == listsplit([1, 1], lambda x: x == 0)
assert [[], [1]] == listsplit([0, 1], lambda x: x == 0)
assert [[1], [1]] == listsplit([1, 0, 1], lambda x: x == 0)
assert [[1, 2], [3, 4, 5], [6, 7]] == listsplit([1, 2, 0, 3, 4, 5, 0, 6, 7], lambda x: x == 0)

def listsplitinc(os, p):
  if len(os) == 0:
    return []
  oses = [[]]
  for o in os:
    if p(o):
      oses.append([o])
    else:
      oses[-1].append(o)
  return oses

assert [] == listsplitinc([], lambda x: x == 0)
assert [[1]] == listsplitinc([1], lambda x: x == 0)
assert [[1], [0]] == listsplitinc([1, 0], lambda x: x == 0)
assert [[1, 1]] == listsplitinc([1, 1], lambda x: x == 0)
assert [[], [0, 1]] == listsplitinc([0, 1], lambda x: x == 0)
assert [[1], [0, 1]] == listsplitinc([1, 0, 1], lambda x: x == 0)
assert [[1, 2], [0, 3, 4, 5], [0, 6, 7]] == listsplitinc([1, 2, 0, 3, 4, 5, 0, 6, 7], lambda x: x == 0)

# Make hashable.
def frz(o):
  if type(o) == list:
    return tuple(map(frz, o))
  elif type(o) == dict:
    return frozenset(map(frz, o.items()))
  elif type(o) == tuple:
    return tuple(map(frz, o))
  elif type(o) in [int, float, str, unicode, bool]:
    return o
  else:
    assert False, (o, type(o))

def forkfunc(a, b):
  def wrapped(*args, **kwargs):
    #print 'FORK A'
    aval = a(*args, **kwargs)
    #print 'FORK B'
    bval = b(*args, **kwargs)
    assert aval == bval, (a.__name__, b.__name__, args, kwargs, aval, bval)
    return aval
  return wrapped

assert 4 == forkfunc(lambda a, b: a + b, lambda a, b: b + a)(1, 3)

# Custom keyfunc only looks at args, not kwargs
def cmemoize(keyfunc):
  def decorator(f):
    cache = {}
    stats = { 'hits': 0, 'misses': 0 }

    def wrapped(*args, **kwargs):
      key = frz((args, kwargs)) if keyfunc == None else keyfunc(args)
      #print 'CM', args, f.__name__, key
      #print 'cachelen before', len(cache)
      if key not in cache:
        stats['misses'] += 1
        cache[key] = f(*args, **kwargs)
        #print 'STORE', args, f.__name__, key, kwargs
      else:
        stats['hits'] += 1
        #print 'HIT', args, f.__name__, key, kwargs
      #print 'cachelen after', len(cache)
      #print 'RESULT', args, f.__name__, cache[key]
      return cache[key]
    wrapped.__name__ = f.__name__

    def report():
      print 'memoization', wrapped.__name__, stats
    atexit.register(report)

    return wrapped
  return decorator

def memoize(f):
  return cmemoize(None)(f)

def bmemoize(keyfunc):
  def decorator(f):
    @memoize
    def wrapped_a(*args, **kwargs):
      return f(*args, **kwargs)
    @cmemoize(keyfunc)
    def wrapped_b(*args, **kwargs):
      return f(*args, **kwargs)
    return forkfunc(wrapped_a, wrapped_b)
  return decorator

def group_by(os, f):
  res = collections.defaultdict(list)
  for o in os:
    res[f(o)].append(o)
  return dict(res)

assert {0: [[0, 1], [0, 2]], 1: [[1, 1], [1, 5]], 2: [[2, 8]]} == group_by([[0, 1], [0, 2], [1, 1], [1, 5], [2, 8]], lambda x: x[0])

def until(f, os):
  if len(os) == 0:
    assert False
    #return None
  else:
    v = f(os[0])
    if v != None:
      return v
    else:
      return until(f, os[1:])

_ = 'kjfnsdkjhsdfkjhadfkjghkdfhgkdfjhgdfjhg'
__ = 'afuhfashrfkauhsdkfasdkjfhaksdjfhasdf'

#def match_try(pair, o):
  #bindings = []
  #return bindings if match_try_1
  #pat, binder = pair

def match_try(pair, o):
  #print 'HEY', pair, len(pair)
  pat, binder = pair
  # Wow, stacky!
  bindings = []
  pats = [pat]
  os = [o]
  while not (pats == [] and os == []):
    #print 'HO0', pats
    #print 'HO1', os
    #print 'HO2', bindings
    if (pats == []) != (os == []):
      return None
    ap = pats[-1]
    op = os[-1]
    #print 'AP', ap, op
    if ap == _:
      bindings.append(op)
      pats.pop()
      os.pop()
    elif ap == __:
      pats.pop()
      os.pop()
    else:
      if type(ap) != type(op):
        return None
      elif type(ap) == str:
        if ap != op:
          return None
        pats.pop()
        os.pop()
      elif type(ap) == list:
        if len(ap) != len(op):
          return None
        else:
          pats.pop()
          os.pop()
          pats = pats + ap
          os = os + op
      else:
        assert False
  return binder(*reversed(bindings))

def match(pairs, o):
  return until(lambda pair: match_try(pair, o), pairs)

assert [14, 13, 12] == match([
  [['a', _, 'b', ['c', _, _, 'd']],  lambda a, b, c: [c, b, a]],
], ['a', 12, 'b', ['c', 13, 14, 'd']])
assert [14, 12] == match([
  [['a', _, 'b', ['c', __, _, 'd']],  lambda a, c: [c, a]],
], ['a', 12, 'b', ['c', 13, 14, 'd']])
