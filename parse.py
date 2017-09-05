#!/usr/bin/python

import itertools
from lib import *
import re
import sys

def nest(tokens, pairs):
  lefts = [pair[0] for pair in pairs]
  rights = [pair[1] for pair in pairs]
  arr_stack = [[]]
  for token in tokens:
    if token['src'] in lefts:
      arr_stack.append([token])
    elif token['src'] in rights:
      arr_stack[-1].append(token)
      top = arr_stack.pop()
      arr_stack[-1].append(top)
    else:
      arr_stack[-1].append(token)
  assert len(arr_stack) == 1, arr_stack
  return arr_stack[0]

#assert (nest(['a', 'aa', '(', 'b', 'bb', '(', 'd', ')', '(', 'e', ')', 'f', 'ff', ')', 'c', 'cc'], [('(', ')')]) ==
  #['a', 'aa', ['(', 'b', 'bb', ['(', 'd', ')'], ['(', 'e', ')'], 'f', 'ff', ')'], 'c', 'cc'])

#assert (nest(['a', 'aa', '(', 'b', 'bb', '(', 'd', ')', 'let', 'e', 'in', 'f', 'ff', ')', 'c', 'cc'], [('(', ')'), ('let', 'in')]) ==
  #['a', 'aa', ['(', 'b', 'bb', ['(', 'd', ')'], ['let', 'e', 'in'], 'f', 'ff', ')'], 'c', 'cc'])

token_patterns = [
  { 'type': 'whitespace', 're': '(^\s+)(.*$)' },
  { 'type': 'string', 're': '("(\"|[^\"])*")(.*$)' },
  { 'type': 'identifier', 're': "(^[a-zA-Z0-9_]+)(.*$)" },
  { 'type': 'operator', 're': '(^[=+-_!@$%^&*?]+)(.*$)' },
  { 'type': 'parenthesis', 're': '(^[\(\)]+)*(.*$)' },
]

def tokenize_line(line_number, line):
  tokens = []
  column_number = 0
  while len(line) > 0:
    for token_pattern in token_patterns:
      m = re.match(token_pattern['re'], line, flags=re.S)
      if m:
        #print 'match', token_pattern['re'], line
        tokens.append({
          'type': token_pattern['type'],
          'src': m.group(1),
          'line_number': line_number,
          'column_number': column_number })
        line = m.group(2)
        column_number += len(m.group(1))
        break
    else:
      assert False, ('Bad token', line)
  return tokens

def tokenize(src):
  lines = [line + '\n' for line in src.split('\n')]
  tokens_with_ws = [token for line_number, line in enumerate(lines) for token in tokenize_line(line_number, line)]
  tokens = [token for token in tokens_with_ws if token['type'] != 'whitespace']
  return tokens

src = """
foo a b = let r = 5
              s = 6 + (let b = 5 in b)
           in r + s
"""

nesters = [
  ('(', ')'),
  ('case', 'of'),
  ('let', 'in')
]

tokens = tokenize(src)
sp(tokens)
tokens = nest(tokens, nesters)

sp(tokens)

# Deal with string literal tokens
