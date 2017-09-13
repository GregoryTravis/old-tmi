#!/usr/bin/python

import itertools
from lib import *
import re
import sys
from zoom import *

def mktok(src):
  return { 'src': src, 'line_number': None, 'column_number': None }

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

def is_token(o):
  return type(o) == dict and 'src' in o.keys()

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
    if 'column_number' in token and token['column_number'] != None:
      while current_column < token['column_number']:
        src += ' '
        current_column += 1
    else:
      src += ' '
    src += token['src']
    current_column += len(token['src'])
  return src

# Nests; preserves order.
def group_by_func(os, f):
  if os == []:
    return []
  last_value = f(os[0])
  grouped = [[os[0]]]
  os = os[1:]
  for o in os:
    value = f(o)
    if value != last_value:
      grouped.append([])
      last_value = value
    grouped[-1].append(o)
  return grouped

assert group_by_func([0, 0, 1, 1, 0, 2, 1, 1, 2, 2, 2], lambda x: x) == [[0, 0], [1, 1], [0], [2], [1, 1], [2, 2, 2]]

def _tokens_to_src(tokens):
  # In place
  for i, token in enumerate(tokens):
    if token['line_number'] == None and i > 0:
      token['line_number'] = tokens[i-1]['line_number']
  #sp(group_by_func(tokens, lambda token: token['line_number'])
  return "\n".join([' '.join([token['src'] for token in linegroup]) for linegroup in group_by_func(tokens, lambda token: token['line_number'])])

def apply_each_level(arr, f):
  return [apply_each_level(x, f) if type(x) == list else x for x in f(arr)]

assert (apply_each_level([1, [2, 3], 4], lambda arr: [0] + arr + [9]) ==
  [0, 1, [0, 2, 3, 9], 4, 9])

nesters = [
  ('(', ')'),
  #('case', 'of'),
  #('let', 'in')
]

def preprocess_file(filename):
  with open(filename, 'r') as f:
    preprocess_src(f.read())

def srceq(s):
  return lambda token: type(token) == dict and token['src'] == s

def peq(s):
  return lambda token: token == s

assert srceq('in')({ 'src': 'in' })
assert not srceq('in')({ 'src': 'inn' })

def find_token_or(a, p):
  for i, x in enumerate(a):
    if p(x):
      return i
  return None

def find_token(a, p):
  r = find_token_or(a, p)
  if r == None:
    assert False, ("Can't find", a, p)
  return r

assert find_token('a b'.split(' '), peq('a')) == 0
assert find_token('a b'.split(' '), peq('b')) == 1
assert find_token('a a'.split(' '), peq('a')) == 0
assert find_token('a b a c d'.split(' '), peq('a')) == 0
assert find_token('a b c b d'.split(' '), peq('b')) == 1
assert find_token_or('a b c b d'.split(' '), peq('q')) == None

def find_token_rtol_or(a, p):
  for i, x in enumerate(a[::-1]):
    if p(x):
      return len(a)-1-i
  return None

def find_token_rtol(a, p):
  r = find_token_rtol_or(a, p)
  if r == None:
    assert False, ("Can't find", a, p)
  return r

assert find_token_rtol('a a'.split(' '), peq('a')) == 1
assert find_token_rtol('a b a c d'.split(' '), peq('a')) == 2
assert find_token_rtol('a b c b d'.split(' '), peq('b')) == 3
assert find_token_rtol_or('a b c b d'.split(' '), peq('q')) == None

def tokline(tokens):
  return ' '.join([token['src'] for token in unnest(tokens)])

def dedenter(column_number, line_number):
  return lambda token: token['line_number'] > line_number and token['column_number'] < column_number

def find_let_in(token):
  return type(token) == list and token[0]['src'] == 'let' and token[-1]['src'] == 'in'

def wrap_in_body(a):
  #in_i = find_token_rtol(a, srceq('in'))
  let_in_i = find_token_rtol_or(a, find_let_in)
  if let_in_i == None:
    return a
  rest = a[let_in_i + 1:]
  body_start = rest[0]
  dedent = find_token_or(rest, dedenter(body_start['column_number'], body_start['line_number']))
  if dedent == None:
    # End
    return wrap_in_body(a[0:let_in_i]) + a[let_in_i] + [[mktok('(')] + rest + [mktok(')')]]
  else:
    # Dedent
    return wrap_in_body(a[0:let_in_i]) + a[let_in_i] + [[mktok('(')] + rest[0:dedent] + [mktok(')')]] + rest[dedent:]

def srceq(s):
  return lambda token: is_token(zval1(token)) and zval1(token)['src'] == s

def find_dedent(tokens, column_number, line_number):
  return zfind1(tokens, lambda z: zval1(z)['column_number'] < column_number and zval1(z)['line_number'] > line_number)

def add_endwhere(tokens):
  z = zmake(tokens)
  wh = zfind1(z, srceq('where'))
  #print 'wh', zval(wh)
  ded = find_dedent(zright(wh), zval1(wh)['column_number'], zval1(wh)['line_number'])
  #print 'ded', zval(ded)
  return zbottom(zwrite(zflatten(ded), [mktok('endwhere')]))

def preprocess_src(src):
  print src
  print '=============='
  tokens = tokenize(src)
  #sp(tokens)
  tokens = nest(tokens, nesters)
  tokens = add_endwhere(tokens)
  #tokens = wrap_in_body(tokens)
  #sp(tokens)
  tokens = unnest(tokens)
  #sp(tokens)
  print tokens_to_src(tokens)

preprocess_file('input.tmi')
# !! Don't like the zflatten in add_endwhere, write should work on taller stacks
# !! Want to be able to re-use 'wh' after the write.  But try finding a second where before the first one and getting two places to write endwhere
