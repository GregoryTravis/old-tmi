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

def unnest(tokens):
  if tokens == []:
    return []
  elif type(tokens[0]) == list:
    return unnest(tokens[0] + tokens[1:])
  else:
    return [tokens[0]] + unnest(tokens[1:])

assert unnest([0, [1, [2], {'a': 12}, 3], 4]) == [0, 1, 2, {'a': 12}, 3, 4]

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

def tokens_to_src(tokens):
  #return concat_tokens(join_pred(tokens, lambda at, bt: space if at['line_number'] == bt['line_number'] else newline))
  src = ''
  current_line = 0
  current_column = 0
  for token in tokens:
    if current_line == token['line_number'] and current_column != 0:
      src += ' '
      current_column += 1
    while current_line < token['line_number']:
      src += '\n'
      current_line += 1
      current_column = 0
    if 'column_number' in token:
      while current_column < token['column_number']:
        src += ' '
        current_column += 1
    src += token['src']
    current_column += len(token['src'])
  return src

def apply_each_level(arr, f):
  return [apply_each_level(x, f) if type(x) == list else x for x in f(arr)]

assert (apply_each_level([1, [2, 3], 4], lambda arr: [0] + arr + [9]) ==
  [0, 1, [0, 2, 3, 9], 4, 9])

nesters = [
  ('(', ')'),
  ('case', 'of'),
  ('let', 'in')
]

def preprocess_file(filename):
  with open(filename, 'r') as f:
    preprocess_src(f.read())

def preprocess_src(src):
  print src
  tokens = tokenize(src)
  #sp(tokens)
  tokens = nest(tokens, nesters)
  #sp(tokens)
  tokens = unnest(tokens)
  print tokens_to_src(tokens)

preprocess_file('input.tmi')
