# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

from ast import *
from lib import *

def pd(t): print dump(t)

def cj(os): return listjoin(os, ', ')
def nlj(os): return listjoin(os, '\n')
def spj(os): return listjoin(os, ' ')

#def indent(os): return ['  ', listjoin(os, '  '), '\n']
def indentlines(os): return ['  ', listjoin(os, '\n  '), '\n']

def compile_exp(t):
  if type(t) == Call:
    return [t.func.id, '(', cj(map(compile_exp, t.args)), ')']
  elif type(t) == Str:
    return ['\'', t.s, '\'']
  elif type(t) == Name:
    return t.id
  elif type(t) == Num:
    return t.n
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
  assert type(t) == Expr
  t = t.value
  assert len(t.ops) == 1 and type(t.ops[0]) == Lt
  assert len(t.comparators) == 1 and type(t.comparators[0].op) == USub
  lhs = t.left
  rhs = t.comparators[0].operand
  return ['write(', compile_exp(lhs), ', ', compile_exp(rhs), ')']

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
s(main())
"""

src = prelude + flatten(compile_body(t)) + postlude
print '------'
print src
with open(srcfile+'.py', 'w') as f:
  f.write(src)
print '------'
