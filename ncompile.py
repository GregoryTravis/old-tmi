# These two lines must be first, or it shall be too late.
import sys
sys.dont_write_bytecode = True

from grako.ast import AST
from grammar import TMIParser

def compile(ast):
  print type(ast), len(ast)
  print ast

filename = sys.argv[1]
with open(filename) as f:
  text = f.read()
parser = TMIParser()
trace = False
ast = parser.parse(text, 'start', filename=filename, trace=trace)
compile(ast)
