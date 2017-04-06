# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

from grako.ast import AST
from grako.contexts import Closure
from grammar import TMIParser
from pprint import pformat

def dump_ast(ast):
  print type(ast), ast
  if type(ast) == Closure or type(ast) == list:
    return ' '.join([dump_ast(ast) for ast in ast])
  elif type(ast) == AST:
    return '(' + ast.parseinfo.rule + ' '.join([k + ': ' + dump_ast(ast) for k, ast in ast.iteritems() if k != 'parseinfo']) + ')'
  elif type(ast) in [str, unicode]:
    return ast
  elif ast == None:
    return 'None'
  else:
    assert False, type(ast)
    return str(ast)

def yah(ast):
  if type(ast) in [Closure, list]:
    return [yah(ast) for ast in ast]
  elif type(ast) == AST:
    return dict({'type': ast.parseinfo.rule}.items() + {k: yah(v) for k, v in ast.iteritems() if k != 'parseinfo'}.items())
  else:
    return ast

def compile(ast):
  print ast
  print pformat(yah(ast))

filename = sys.argv[1]
with open(filename) as f:
  text = f.read()
parser = TMIParser()
trace = True
ast = parser.parse(text, 'start', filename=filename, trace=trace)
compile(ast)

"""
- left recursion problem
  - https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion
- parse fact()
"""
