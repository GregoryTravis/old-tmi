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

def unimplementedBackwards():
  raise NotImplementedError

def check_node_arg_lists(cls):
  forwards = cls.forwards
  fargs = inspect.getargspec(forwards)
  assert fargs.varargs == None
  assert fargs.keywords == None
  assert fargs.defaults == None

  if issubclass(cls, UNode):
    return

  backwards = cls.backwards
  bargs = inspect.getargspec(backwards)
  assert ['out'] + fargs.args == bargs.args, (fargs.args, bargs.args)
  assert bargs.varargs == None
  assert bargs.keywords == None
  assert bargs.defaults == None

def check_argspec_match(forwardsFunction, (args, kwargs)):
  assert len(kwargs) == 0
  assert len(inspect.getargspec(forwardsFunction).args) == len(args)

def isnode(n): return issubclass(type(n), Node)

def nodeLift(o): return o if isnode(o) else Constant(o)

class Node(object):
  def __repr__(self):
    return type(self).__name__ + '(' + ', '.join(map(str, self.args)) + ')'
  def __str__(self): return self.__repr__()

def node(cls):
  assert issubclass(cls, Node)

  # Create and install ctor
  def ctor(self, *args, **kwargs):
    check_argspec_match(cls.forwards, (args, kwargs))
    self.args = map(nodeLift, args)
  cls.__init__ = ctor

  # Check f/b argspec compatibility
  check_node_arg_lists(cls)

  # read() and write()

  cls.forwards = staticmethod(cls.forwards)
  cls.backwards = staticmethod(cls.backwards)

  return cls

class UNode(Node):
  @staticmethod
  def backwards():
    raise NotImplementedError

def Constant(o):
  @node
  class Constant_(UNode):
    @staticmethod
    def forwards():
      return o
    def __repr__(self):
      return str(o)
    def __str__(self): return self.__repr__()
  return Constant_()

assert isnode(Constant(1))
assert not isnode(1)

@node
class Where(Node):
  def forwards(rel, pred):
    return [rec for rec in rel if pred(rec)]

  def backwards(out, rel, pred):
    assert all(map(pred, out))
    return where(rel, pnot(pred)) + out
 
assert 12 == Constant(12).forwards()

w = Where(r, ceq('a', 1))
print w

#assert [d(a=1, b=2)] == where(r, ceq('a', 1))
#assert [d(a=10, b=20)] == where(r, pnot(ceq('a', 1)))

