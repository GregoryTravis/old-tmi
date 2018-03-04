# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

from ast import *
from lib import *

def err(s):
  print 'Error:\n  %s' % s
  sys.exit(1)

def foldr(f, e, xs):
  return e if len(xs) == 0 else f(xs[0], foldr(f, e, xs[1:]))
assert [1, [2, [3, [4, [5, []]]]]] == foldr((lambda a, b: [a, b]), [], (1, 2, 3, 4, 5))

def pd(t): print dump(t)

def cj(os): return listjoin(os, ', ')
def nlj(os): return listjoin(os, '\n')
def spj(os): return listjoin(os, ' ')
def s(s): return ['\'', s, '\'']

#def indent(os): return ['  ', listjoin(os, '  '), '\n']
def indentlines(os): return ['  ', listjoin(os, '\n  '), '\n']

def compile_attribute_expression(t):
  if t.attr == '_':
    return ['One(', compile_exp(t.value), ')']
  else:
    return ['Deref(', compile_exp(t.value), ', ', s(t.attr), ')']

def compile_predexp_clause(t):
  pd(t)
  assert type(t) == Compare and len(t.ops) == 1 and type(t.ops[0]) == Eq and len(t.comparators) == 1
  assert type(t.left) == Name
  return ['Feq(', s(t.left.id), ', ', compile_exp(t.comparators[0]), ')']

def compile_predexp(t):
  if type(t) == Compare:
    return compile_predexp_clause(t)
  assert type(t) == Tuple
  assert len(t.elts) >= 2
  celts = map(compile_predexp_clause, t.elts)
  return foldr((lambda a, b: ['Pand(', a, ', ', b, ')']), celts[-1], celts[:-1])

def compile_where_expression(t):
  assert type(t) == BinOp and type(t.op) == Div
  rel = t.left
  pred = compile_predexp(t.right)
  return ['Where(', compile_exp(rel), ', ', pred, ')']

def compile_exp(t):
  if type(t) == Call:
    return [t.func.id, '(', cj(map(compile_exp, t.args)), ')']
  elif type(t) == Str:
    return s(t.s)
  elif type(t) == Name:
    return t.id
  elif type(t) == Num:
    return t.n
  elif type(t) == Attribute:
    return compile_attribute_expression(t)
  elif type(t) == BinOp and type(t.op) == Div:
    return compile_where_expression(t)
  else:
    assert False, (t, dump(t))

def compile_native_assignment(t):
  assert len(t.targets) == 1
  return spj([t.targets[0].id, '=', compile_exp(t.value)])

def compile_name(t):
  assert type(t) == Name
  return t.id

def compile_retexp(t):
  assert type(t) == Expr
  return ['return ', compile_exp(t.value)]

def compile_write(t):
  if type(t) == Assign:
    err('You mean <- here')
  assert type(t) == Expr, t
  t = t.value
  assert len(t.ops) == 1 and type(t.ops[0]) == Lt
  lhs = t.left
  assert len(t.comparators) == 1
  if type(t.comparators[0]) == UnaryOp and type(t.comparators[0].op) == USub:
    # The most common format, where (a <- b) is parsed as (a < -(b))
    rhs = compile_exp(t.comparators[0].operand)
  elif type(t.comparators[0]) == Num:
    # If the rhs is a number, then (a <- 12) is parsed as (a < -12) where -12 is a constant Num
    rhs = -(t.comparators[0].n)
  else:
    assert False, dump(t)
  return ['write(', compile_exp(lhs), ', ', rhs, ')']

def compile_def_body(ts):
  return indentlines(map(compile_write, ts[:-1]) + [compile_retexp(ts[-1])])

def compile_def(t):
  assert t.args.vararg == None
  assert t.args.kwarg == None
  assert t.args.defaults == []
  return ['def ', t.name, '(', cj(map(compile_name, t.args.args)), '):\n',
    compile_def_body(t.body)]

def compile_body_element(t):
  if type(t) == Assign:
    return compile_native_assignment(t)
  elif type(t) == FunctionDef:
    return compile_def(t)
  else:
    assert False, t

def compile_body(t):
  return nlj(map(compile_body_element, t.body))

srcfile = sys.argv[1]
with open(srcfile, 'r') as f:
  src = f.read()
t = parse(src, srcfile)

prelude = """
import sys
sys.dont_write_bytecode = True
from tmi import *
from web import link, mkform, Cookies, redirect, ListJoin
from tags import *

"""

postlude = """
v = main()
commit()
s(v)
"""

src = prelude + flatten(compile_body(t)) + postlude
print '------'
print src
with open(srcfile+'.py', 'w') as f:
  f.write(src)
print '------'
