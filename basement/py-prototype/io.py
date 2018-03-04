import ast
import pprint

def readdat(filename):
  with open(filename) as f:
    return ast.literal_eval(f.read())

def writedat(filename, data):
  with open(filename, 'w') as f:
    f.write(pprint.pformat(data, width=150))
