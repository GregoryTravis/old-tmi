# -*- coding: UTF-8 -*-
import inspect
from tabulate import tabulate

def d(**kwargs):
  return dict(**kwargs)

def ss(rel):
  cols = rel[0].keys() if len(rel) > 0 else ['']
  return tabulate([[rec[f] for f in cols] for rec in rel], cols, tablefmt='fancy_grid')
assert ss([d(a=1, b=2), d(a=10, b=20)]) == u'╒═════╤═════╕\n│   a │   b │\n╞═════╪═════╡\n│   1 │   2 │\n├─────┼─────┤\n│  10 │  20 │\n╘═════╧═════╛'

def s(*args):
  for o in args:
    print ss(o)
  return args[-1]

r = [
  d(a=1, b=2),
  d(a=10, b=20),
]

# True if equal to srec on all fields of srec
def sreceq(srec): return lambda rec: all([rec[col] == srec[col] for col in srec.keys()])

assert sreceq(d(a=1))(d(a=1, b=2))
assert sreceq(d(a=1))(d(a=1, b=2, c=3))
assert sreceq(d(c=3))(d(a=1, b=2, c=3))
assert not sreceq(d(a=2))(d(a=1, b=2))

def ceq(col, value): return sreceq({ col: value })
assert ceq('a', 1)(d(a=1, b=2))
assert ceq('a', 1)(d(a=1, b=2, c=3))
assert ceq('c', 3)(d(a=1, b=2, c=3))
assert not ceq('a', 2)(d(a=1, b=2))

def pnot(pred): return lambda x: not pred(x)
assert not pnot(ceq('a', 1))(d(a=1, b=2))
assert not pnot(ceq('a', 1))(d(a=1, b=2, c=3))
assert not pnot(ceq('c', 3))(d(a=1, b=2, c=3))
assert pnot(ceq('a', 2))(d(a=1, b=2))

def where(rel, pred):
  return [rec for rec in rel if pred(rec)]
assert [d(a=1, b=2)] == where(r, ceq('a', 1))
assert [d(a=10, b=20)] == where(r, pnot(ceq('a', 1)))

def un_where(out, rel, pred):
  assert all(map(pred, out))
  return where(rel, pnot(pred)) + out

def unimplementedBackwards():
  raise 'UnimplementedBackwards'

def check_node_arg_lists(forwards, backwards):
  fargs = inspect.getargspec(forwards)
  assert fargs.varargs == None
  assert fargs.keywords == None
  assert fargs.defaults == None
  if backwards == unimplementedBackwards:
    return
  bargs = inspect.getargspec(backwards)
  assert ['out'] + fargs.args == bargs.args, (fargs.args, bargs.args)
  assert bargs.varargs == None
  assert bargs.keywords == None
  assert bargs.defaults == None

class Node(object):
  def __init__(self, nodeConstructor, arglist):
    self.nodeConstructor = nodeConstructor
    self.arglist = arglist

  def __str__(self):
    return type(self.nodeConstructor).__name__ + '(' + ', '.join(map(str, self.arglist)) + ')'

  def __repr__(self): return self.__str__()

class NodeConstructor(object):
  def __init__(self, forwards, backwards):
    check_node_arg_lists(forwards, backwards)
    self.forwards = forwards;
    self.backwards = backwards;

  def __str__(self):
    return type(self).__name__ + '(' + ', '.join(inspect.getargspec(self.forwards).args) + ')'

  def __repr__(self): return self.__str__()

  def __call__(self, *args, **kwargs):
    assert len(kwargs) == 0
    return Node(self, list(args))

def UNodeConstructor(forwards):
  return NodeConstructor(forwards, unimplementedBackwards)

def Constant(x):
  return UNodeConstructor(lambda: x)()

def isnode(n): return type(n) == Node

Where = NodeConstructor(where, un_where)
assert 'NodeConstructor(rel, pred)' == str(Where)
assert isnode(Where(1))
assert not isnode(12)

print Where(1)
print Constant(2)
# How to give nodes a name?  How about make them classes with two methods, then post-process (and an error if you don't)
# Lift constants
