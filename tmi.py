# -*- coding: UTF-8 -*-
import inspect
from collections import defaultdict, deque
from tabulate import tabulate

def d(**kwargs):
  return dict(**kwargs)

def ss(rel):
  cols = rel[0].keys() if len(rel) > 0 else ['']
  return tabulate([[rec[f] for f in cols] for rec in rel], cols, tablefmt='fancy_grid')
assert ss([d(a=1, b=2), d(a=10, b=20)]) == u'╒═════╤═════╕\n│   a │   b │\n╞═════╪═════╡\n│   1 │   2 │\n├─────┼─────┤\n│  10 │  20 │\n╘═════╧═════╛'

trace_indentation = 0
def trace(f):
  def wrapped(*args, **kwargs):
    global trace_indentation
    prefix = '| ' * trace_indentation
    assert len(kwargs) == 0
    print prefix + '-- ' + f.__name__ + '(' + ', '.join(map(str, args)) + ')'
    trace_indentation += 1
    ret = f(*args, **kwargs)
    trace_indentation -= 1
    print prefix + '-> ' + str(ret)
    return ret
  return wrapped

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

  backwards = cls.backwards
  bargs = inspect.getargspec(backwards)
  assert ['out'] + fargs.args == bargs.args, (fargs.args, bargs.args)
  assert bargs.varargs == None
  assert bargs.keywords == None
  assert bargs.defaults == None

def check_argspec_match(forwardsFunction, (args, kwargs)):
  assert len(kwargs) == 0
  assert len(inspect.getargspec(forwardsFunction).args) == len(args)

node_serial = 0
def get_node_serial():
  global node_serial
  ret = node_serial
  node_serial += 1
  return ret

def isnode(n): return issubclass(type(n), Node)

def nodeLift(o): return o if isnode(o) else Constant(o)

#@trace
def read(node):
  forwards = node.forwards
  return forwards(*map(read, node.args))

writes = deque()
#@trace
def write(node, value):
  global writes
  writes.append((node, value))

#@trace
def propagateOne(node, value):
  indict = node.backwards(*([value] + map(read, node.args)))
  forargs = inspect.getargspec(node.forwards).args
  # TODO: require all inputs to be written to?
  # assert set(forargs) == indict.keys()
  return [(node.args[forargs.index(arg)], value) for arg, value in indict.iteritems()]
  #return [(node.args[i], indict[arg]) for i, arg in enumerate(forargs)]

def commit():
  global writes

  # Propagate to the left
  accum = defaultdict(list)
  while len(writes) > 0:
    (node, value) = writes.popleft()
    accum[node.serial].append(value)
    writes.extend(propagateOne(node, value))

  # TODO: check for duplicates
  # Only one value per node.
  assert all([len(values) < 2 for serial, values in accum.iteritems()]), list(accum.iteritems())

  # - write node 0 to diszk
  # print 'FINAL', accum

  # Reset
  writes = defaultdict(list)

class Node(object):
  def __repr__(self):
    return type(self).__name__ + '(' + ', '.join(map(str, self.args)) + ')'
  def __str__(self): return self.__repr__()

def node(cls):
  assert issubclass(cls, Node)

  # Create and install ctor
  def ctor(self, *args, **kwargs):
    check_argspec_match(cls.forwards, (args, kwargs))
    self.serial = get_node_serial()
    self.args = map(nodeLift, args)
  cls.__init__ = ctor

  # Check f/b argspec compatibility
  check_node_arg_lists(cls)

  cls.forwards = staticmethod(cls.__dict__['forwards'])
  cls.backwards = staticmethod(cls.__dict__['backwards'])

  return cls

class UNode(Node):
  def backwards(out):
    raise NotImplementedError

def Constant(o):
  @node
  class Constant_(UNode):
    def forwards():
      return o
    # Why is this needed?  Without it, node() can't find a backwards method to alter.
    def backwards(out):
      raise NotImplementedError
    def __repr__(self):
      return str(o)
    def __str__(self): return self.__repr__()
  return Constant_()

def Box(o):
  box = [o]
  @node
  class Box_(UNode):
    def forwards():
      return box[0]
    def backwards(out):
      box[0] = out
      return {}
    def __repr__(self):
      return 'Box(' + str(box[0]) + ')'
    def __str__(self): return self.__repr__()
  return Box_()

assert isnode(Constant(1))
assert not isnode(1)

@node
class Where(Node):
  def forwards(rel, pred):
    return [rec for rec in rel if pred(rec)]

  def backwards(out, rel, pred):
    assert all(map(pred, out))
    return d(rel=([rec for rec in rel if not pred(rec)] + out))
 
assert 12 == read(Constant(12))

assert [{'a': 1, 'b': 2}] == read(Where(r, ceq('a', 1)))
assert [{'a': 10, 'b': 20}] == read(Where(r, pnot(ceq('a', 1))))

b = Box(r)
w = Where(b, ceq('a', 1))
write(w, [d(a=1, b=200)])
commit()
print 'haha', read(b)
