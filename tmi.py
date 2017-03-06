# -*- coding: UTF-8 -*-
import inspect
import itertools
import json
import operator
import os

D = dict

from collections import defaultdict, deque
from tabulate import tabulate

def d(**kwargs):
  return dict(**kwargs)

def rel_renderer(rel):
  if type(rel) != list or not all([type(row) == dict for row in rel]): return None
  cols = rel[0].keys() if len(rel) > 0 else ['']
  return tabulate([[rec[f] for f in cols] for rec in rel], cols, tablefmt='fancy_grid')
assert (u'╒═════╤═════╕\n│   a │   b │\n╞═════╪═════╡\n│   1 │   2 │\n├─────┼─────┤\n│  10 │  20 │\n╘═════╧═════╛' ==
  rel_renderer([d(a=1, b=2), d(a=10, b=20)]))

def node_renderer(node):
  if not isnode(node): return None
  return ss(read(node))

ss_renderers = [
  rel_renderer,
  node_renderer,
]

def ss(o):
  for renderer in ss_renderers:
    s = renderer(o)
    if s: return s
  return str(o)

def ss_install_renderer(renderer):
  global ss_renderers
  ss_renderers.append(renderer)

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

def check_node_arg_lists(cls):
  forwards = cls.forwards
  fargs = inspect.getargspec(forwards)
  assert fargs.keywords == None
  assert fargs.defaults == None

  if not issubclass(cls, UNode):
    backwards = cls.backwards
    bargs = inspect.getargspec(backwards)
    assert ['out'] + fargs.args == bargs.args, (fargs.args, bargs.args)
    assert bargs.keywords == None
    assert bargs.defaults == None

def check_argspec_match(forwardsFunction, (args, kwargs)):
  assert len(kwargs) == 0
  argspec = inspect.getargspec(forwardsFunction)
  if argspec.varargs == None:
    # Args must exactly match.
    assert len(argspec.args) == len(args)
  else:
    # Args must be at least as many as the non-varargs ones.
    assert len(argspec.args) <= len(args)

node_serial = 0
def get_node_serial():
  global node_serial
  ret = node_serial
  node_serial += 1
  return ret

def isnode(n): return issubclass(type(n), Node)
def isunode(n): return issubclass(type(n), UNode)
def islnode(n): return issubclass(type(n), LNode)

def nodeLift(o): return o if isnode(o) else Constant(o)

def reader(node): return lambda: read(node)

#@trace
def read(node):
  return node.forwards(*[reader(arg) if islnode(node) else read(arg) for arg in node.args])

writes = None

def resetWrites():
  global writes
  writes = deque()
resetWrites()

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
    value = read(value) if isnode(value) else value
    accum[node.serial].append(value)
    if not isunode(node):
      writes.extend(propagateOne(node, value))

  # TODO: check for duplicates
  # Only one value per node.
  assert all([len(values) < 2 for serial, values in accum.iteritems()]), list(accum.iteritems())

  # - write node 0 to disk
  # print 'FINAL', accum

  # Reset
  resetWrites()

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
  if not issubclass(cls, UNode):
    cls.backwards = staticmethod(cls.__dict__['backwards'])

  return cls

# Unidirectional
class UNode(Node):
  pass

# Lazy -- inputs are passed as thunks rather than values
class LNode(UNode):
  pass

def Constant(o):
  @node
  class Constant_(UNode):
    def forwards():
      return o
    # todo Why is this needed?  Without it, node() can't find a backwards method to alter.
    def __repr__(self):
      return str(o)
  return Constant_()

def Box(o):
  box = [o]
  @node
  class Box_(Node):
    def forwards():
      return box[0]
    def backwards(out):
      box[0] = out
      return {}
    def __repr__(self):
      return 'Box(' + str(box[0]) + ')'
  return Box_()

assert isnode(Constant(1))
assert not isnode(1)

assert '1' == str(Constant(1)), str(Constant(1))
assert 'Box(1)' == str(Box(1))

def File(filename):
  @node
  class File_(Node):
    def forwards():
      with open(filename) as f:
        return json.load(f)
    def backwards(out):
      with open(filename, 'w') as f:
        json.dump(out, f)
      return {}
    def __repr__(self):
      return 'File(' + filename + ')'
  return File_()

@node
class Where(Node):
  def forwards(rel, pred):
    return [rec for rec in rel if pred(rec)]

  def backwards(out, rel, pred):
    assert all(map(pred, out))
    return d(rel=([rec for rec in rel if not pred(rec)] + out))

def setfield(d, k, v):
  assert type(d) == dict
  return {kk: v if k == kk else vv for kk, vv in d.iteritems()}

@node
class Deref(Node):
  def forwards(rec, field):
    return rec[field]
  def backwards(out, rec, field):
    return { 'rec': setfield(rec, field, out) }

@node
class HasField(UNode):
  def forwards(rec, field):
    return field in rec.keys()

@node
class List(UNode):
  def forwards(*args):
    return args

@node
class One(Node):
  def forwards(rel):
    assert type(rel) == list
    assert len(rel) == 1
    return rel[0]
  def backwards(out, rel):
    assert type(out) == dict
    return { 'rel': [out] }

assert 12 == read(Constant(12))

assert [{'a': 1, 'b': 2}] == read(Where(r, ceq('a', 1)))
assert [{'a': 10, 'b': 20}] == read(Where(r, pnot(ceq('a', 1))))

b = Box(r)
w = Where(b, ceq('a', 1))
write(w, [d(a=1, b=200)])
commit()
assert [{'a': 10, 'b': 20}, {'a': 1, 'b': 200}] == read(b)

with open('tmp.dat', 'w') as f: f.write('{}')
f = File('tmp.dat')
write(f, read(b)) # TODO shouldn't need read
commit()
f2 = File('tmp.dat')
assert read(b) == read(f2)
os.remove('tmp.dat')

assert {'a': 1, 'b': 200} == read(One(w))
assert 1 == read(Deref(One(w), 'a'))
assert ss(read(w)) == ss(w)

def common_fields(left, right):
  return set(left.keys()).intersection(set(right.keys())) 

assert set(['b', 'c']) == common_fields(dict(a=1, b=2, c=3), dict(b=3, c=4, d=5)), common_fields(dict(a=1, b=2, c=3), dict(b=3, c=4, d=5))
assert set([]) == common_fields(dict(a=1, b=2), dict(c=3, d=4))

def eq_on(rela, relb, fields):
  return all([rela[field] == relb[field] for field in fields])

assert eq_on(dict(a=1, b=2, c=3), dict(b=2, c=3, d=4), ['b', 'c'])
assert not eq_on(dict(a=1, b=2, c=3), dict(b=20, c=3, d=4), ['b', 'c'])
assert not eq_on(dict(a=1, b=2, c=3), dict(b=2, c=30, d=4), ['b', 'c'])
assert eq_on(dict(a=1, b=2, c=3), dict(b=20, c=3, d=4), ['c'])
assert eq_on(dict(a=1, b=2, c=3), dict(b=2, c=30, d=4), ['b'])

# Record union, but may have common fields if they are equal on them
def omerge(rela, relb):
  assert eq_on(rela, relb, common_fields(rela, relb))
  all_fields = set(rela.keys()).union(set(relb.keys()))
  return {k: (rela[k] if k in rela else relb[k]) for k in all_fields}

assert dict(a=1, b=2, c=3, d=4) == omerge(dict(a=1, b=2, c=3), dict(b=2, c=3, d=4))
assert dict(a=1, b=20, c=3, d=4) == omerge(dict(a=1, c=3), dict(b=20, c=3, d=4))
assert dict(a=1, b=2, c=30, d=4) == omerge(dict(a=1, b=2), dict(b=2, c=30, d=4))

@node
class Join(UNode):
  def forwards(left, right):
    if len(left) == 0 or len(right) == 0:
      return []
    join_fields = common_fields(left[0], right[0])
    assert len(join_fields) > 0
    return [omerge(lrow, rrow) for lrow in left for rrow in right if eq_on(lrow, rrow, join_fields)]

db = File('old.dat')
player = Deref(db, 'player')
card = Deref(db, 'card')
game = Deref(db, 'game')
hand = Deref(db, 'hand')

# Make hashable.
def frz(o):
  if type(o) == list:
    return tuple(map(frz, o))
  elif type(o) == dict:
    return frozenset(map(frz, o.items()))
  elif type(o) == tuple:
    return tuple(map(frz, o))
  elif type(o) in [int, float, str, unicode]:
    return o
  else:
    assert False, (o, type(o))

# Two relations are equal, treating the lists as sets.
def releq(rela, relb):
  return set(frz(rela)) == set(frz(relb))

def check_join_commutative(a, b):
  assert releq(read(Join(a, b)), read(Join(b, a)))

def check_join_associative_3(a, b, c):
  assert releq(read(Join(a, Join(b, c))), read(Join(Join(a, b), c)))

#def check_join_associative_4(a, b, c, d):
  #join_associative_3(a, b, Join(c, d))
  #join_associative_3(a, Join(b, c), d)
  #join_associative_3(Join(a, b), c, d)

def same(os, comparator=operator.eq):
  if len(os) < 2:
    return True
  return all([comparator(os[0], o) for o in os[1:]])

assert same([1, 1, 1, 1])
assert same([1, 1])
assert same([1])
assert same([])
assert not same([2, 1, 1, 1])
assert not same([2, 1])
assert not same([[dict(a=1, b=2), dict(c=3, d=4)], [dict(c=3, d=4), dict(a=1, b=2)]])
assert same([[dict(a=1, b=2), dict(c=3, d=4)], [dict(c=3, d=4), dict(a=1, b=2)]], releq)

def chain_join(rels):
  assert len(rels) > 0
  if len(rels) == 1:
    return rels[0]
  else:
    return Join(chain_join(rels[:-1]), rels[-1])

# TODO Rid of these reads
assert read(Join(Join(Join(hand, game), player), card)) == read(chain_join([hand, game, player, card]))
assert read(Join(hand, game)) == read(chain_join([hand, game]))
assert read(hand) == read(chain_join([hand]))

# All orderings of a star join configuration: a center (that must be part of
# any join) and branches (which can come in any order).
def check_star_join_permutations(center, branches):
  branch_permutations = map(list, itertools.permutations(branches))
  # The center must be the first or second in the list
  permutations = [[center] + bp for bp in branch_permutations] + [[bp[0]] + [center] + bp[1:] for bp in branch_permutations]
  assert same(map(chain_join, permutations), lambda a, b: releq(read(a), read(b)))

check_star_join_permutations(hand, [player, game, card])

# TODO this is not comprehensive.

check_join_commutative(player, hand)
check_join_commutative(game, hand)
check_join_commutative(card, hand)
check_join_commutative(card, Join(hand, player))
check_join_commutative(card, Join(hand, game))
check_join_commutative(card, Join(player, Join(hand, game)))
check_join_commutative(player, Join(card, Join(hand, game)))
check_join_commutative(game, Join(player, Join(hand, card)))

check_join_associative_3(player, hand, card)
check_join_associative_3(player, Join(game, hand), card)
check_join_associative_3(game, Join(player, hand), card)
check_join_associative_3(game, Join(card, hand), player)

def remove_duplicates(os):
  seen = set()
  out = []
  for o in os:
    fo = frz(o)
    if fo not in seen:
      seen.add(fo)
      out.append(o)
  return out

assert [1, 2, 3] == remove_duplicates([1, 2, 1, 1, 2, 2, 1, 1, 2, 3, 3, 3])

@node
class Proj(UNode):
  def forwards(rel, fields):
    return remove_duplicates([{k: rec[k] for k in fields} for rec in rel])

la = [
  D(a=1, b=2, c=3),
  D(a=1, b=2, c=30),
  D(a=1, b=2, c=31),
  D(a=1, b=20, c=3),
  D(a=1, b=20, c=30),
  D(a=1, b=20, c=31),
  D(a=10, b=20, c=30),
]
assert releq([D(a=1, b=2), D(a=1, b=20), D(a=10, b=20)], read(Proj(la, ['a', 'b'])))
assert releq([D(a=1), D(a=10)], read(Proj(la, ['a'])))
assert releq([D(b=2), D(b=20)], read(Proj(la, ['b'])))
assert 3 == len(read(Proj(la, ['a', 'b'])))
assert 2 == len(read(Proj(la, ['a'])))
assert 2 == len(read(Proj(la, ['b'])))

@node
class If(LNode):
  def forwards(b, t, e):
    b = b()
    assert type(b) == bool
    return t() if b else e()

assert 2 == read(If(True, 2, 3))
assert 3 == read(If(False, 2, 3))

@node
class And(LNode):
  def forwards(a, b):
    return a() and b()

assert read(And(True, True))
assert not read(And(True, False))
assert not read(And(False, True))
assert not read(And(False, False))

@node
class Not(UNode):
  def forwards(b):
    return not b

@node
class Equals(UNode):
  def forwards(a, b):
    return a == b

assert read(And(Equals(2, 2), Not(Equals(2, 3))))
