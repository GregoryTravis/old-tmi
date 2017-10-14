#!/usr/bin/python

import itertools
from lib import *
from pprint import pformat, pprint
from throob import parse_top, srcish, is_token, gram
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
  { 'type': 'let_keyword', 're': '(^let)(.*$)' },
  { 'type': 'in_keyword', 're': '(^in)(.*$)' },
  { 'type': 'where_keyword', 're': '(^where)(.*$)' },
  { 'type': 'identifier', 're': "(^[a-zA-Z0-9_]+)(.*$)" },
  { 'type': 'semicolon', 're': '(^;)(.*$)' },
  { 'type': 'equals', 're': "(^=)(.*$)" },
  { 'type': 'operator', 're': '(^[=<>+\-_!@$%^&*?]+)(.*$)' },
  { 'type': 'lparen', 're': '(^[\(])(.*$)' },
  { 'type': 'rparen', 're': '(^[\)])(.*$)' },
  { 'type': 'lcb', 're': '(^[\{])(.*$)' },
  { 'type': 'rcb', 're': '(^[\}])(.*$)' },
]

def toktype(type):
  return lambda token: is_token(token) and token['type'] == type

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
def group_by_po_func(os, f):
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

assert group_by_po_func([0, 0, 1, 1, 0, 2, 1, 1, 2, 2, 2], lambda x: x) == [[0, 0], [1, 1], [0], [2], [1, 1], [2, 2, 2]]

def _tokens_to_src(tokens):
  # In place
  for i, token in enumerate(tokens):
    if token['line_number'] == None and i > 0:
      token['line_number'] = tokens[i-1]['line_number']
  #sp(group_by_po_func(tokens, lambda token: token['line_number'])
  return "\n".join([' '.join([token['src'] for token in linegroup]) for linegroup in group_by_po_func(tokens, lambda token: token['line_number'])])

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
  print map(zval, zfindall1(z, srceq('where')))
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

#preprocess_file('input.tmi')

# !! Don't like the zflatten in add_endwhere, write should work on taller stacks
# !! Should just get all such spots.  Now printing out all endwhere spots, but don't have writes yet.
# !! Then do nesting and thus try (b where b = 70), the non-dedent spot -- haven't implemented that yet
# !! Ooo wouldn't have to worry about getting the order of endwhere/endin wrong when merging overlapping writes, just use end for all!

def hya(src):
  #print src
  #print '=============='
  tokens = tokenize(src)
  indent_stack = []
  output = []
  inx = 0

  indent_stack.append(('let', 0, 0))

  while inx < len(tokens):
    current = tokens[inx]
    #print 'aaa', current, indent_stack

    # dedent
    while True:
      #print '-', inx, current, indent_stack
      if len(indent_stack) > 0 and indent_stack[-1][1] > current['column_number'] and indent_stack[-1][2] < current['line_number']:
        #print 'dedent', current, indent_stack[-1]
        if indent_stack[-1][0] == 'let':
          assert current['src'] == 'in', ('Expected in', current, indent_stack)
          break
          #output.append(mktok('}'))
          #indent_stack.pop()
        elif indent_stack[-1][0] == 'where':
          output.append(mktok('}'))
          indent_stack.pop()
        elif indent_stack[-1][0] == 'case':
          assert current['src'] == 'of'
          #indent_stack.pop()
          break
        elif indent_stack[-1][0] == 'of':
          output.append(mktok('}'))
          indent_stack.pop()
          assert indent_stack[-1][0] == 'case'
          indent_stack.pop()
        else:
          assert False, '???'
      else:
        break

    # eqdent
    if len(indent_stack) > 0 and indent_stack[-1][1] == current['column_number'] and indent_stack[-1][2] < current['line_number']:
      if indent_stack[-1][0] == 'let' or indent_stack[-1][0] == 'where' or indent_stack[-1][0] == 'of':
        output.append(mktok(';'))
      else:
        assert False, ('Bad dedent?', indent_stack)

    #print current, indent_stack
    if False:
      assert False
    elif current['src'] == '(':
      indent_stack.append(('(', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
      output.append(current)
    elif current['src'] == ')':
      while indent_stack[-1][0] in ['where', 'of']:
        if indent_stack[-1][0] == 'where':
          output.append(mktok('}'))
          indent_stack.pop()
        elif indent_stack[-1][0] == 'of':
          output.append(mktok('}'))
          indent_stack.pop()
          assert indent_stack[-1][0] == 'case'
          indent_stack.pop()
        else:
          assert False
      assert indent_stack[-1][0] == '(', indent_stack
      indent_stack.pop()
      output.append(current)
    elif current['src'] == 'in':
      assert len(indent_stack) > 0, ('initial in', current, indent_stack)
      assert indent_stack[-1][0] == 'let', ('mismatched in', current, indent_stack)
      indent_stack.pop()
      output.append(mktok('}'))
      output.append(current)
    elif current['src'] == 'let':
      assert inx + 1 < len(tokens), 'Dangling let'
      indent_stack.append(('let', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
      output.append(current)
      output.append(mktok('{'))
    elif current['src'] == 'where':
      assert inx + 1 < len(tokens), 'Dangling where'
      indent_stack.append(('where', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
      output.append(current)
      output.append(mktok('{'))
    elif current['src'] == 'case':
      assert inx + 1 < len(tokens), 'Dangling case'
      indent_stack.append(('case', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
      output.append(current)
    elif current['src'] == 'of':
      assert inx + 1 < len(tokens), 'Dangling of'
      indent_stack.append(('of', tokens[inx+1]['column_number'], tokens[inx+1]['line_number']))
      output.append(current)
      output.append(mktok('{'))
    else:
#      # eqdent
#      if len(indent_stack) > 0 and indent_stack[-1][1] == current['column_number'] and indent_stack[-1][2] < current['line_number']:
#        if indent_stack[-1][0] == 'let' or indent_stack[-1][0] == 'where':
#          output.append(mktok(';'))
#        else:
#          assert False, ('Bad dedent?', indent_stack)
#      # dedent
#      elif len(indent_stack) > 0 and indent_stack[-1][1] >= current['column_number'] and indent_stack[-1][2] < current['line_number']:
#        if indent_stack[-1][0] == 'let':
#          assert False, ('Should have caught in', current, indent_stack)
#        elif indent_stack[-1][0] == 'where':
#          output.append(mktok('}'))
#          indent_stack.pop()
#        else:
#          assert False, '???'
      output.append(current)
    inx += 1

  # Implicit final dedent
  while len(indent_stack) > 1:
    if indent_stack[-1][0] == 'let':
      assert False, 'dangling let'
    elif indent_stack[-1][0] == 'where':
      output.append(mktok('}'))
      indent_stack.pop()
    elif indent_stack[-1][0] == 'case':
      assert False, 'dangling case'
    elif indent_stack[-1][0] == 'of':
      output.append(mktok('}'))
      indent_stack.pop()
      assert indent_stack[-1][0] == 'case'
      indent_stack.pop()
    else:
      assert False, '???'

  assert len(indent_stack) == 1, indent_stack

  return tokens_to_src(output)

def nest_cbs(tokens):
  st = [[]]
  for token in tokens:
    if token['type'] == 'lcb':
      st.append([])
      st[-1].append(token)
    elif token['type'] == 'rcb':
      assert st[-1][0]['type'] == 'lcb'
      st[-1].append(token)
      block = st.pop()
      st[-1].append(block)
    else:
      st[-1].append(token)
  assert len(st) == 1
  return st[0]

def split_blocks(tokens):
  st = [[]]
  if tokens[0]['type'] == 'lcb' and tokens[-1]['type'] == 'rcb':
    return [split_blocks(decl) for decl in listsplit(tokens[1:-1], toktype('semicolon'))]
  else:
    return [e if is_token(e) else split_blocks(e) for e in tokens]

def parse(presrc):
  tokens = tokenize(presrc)
  print 'tokens', len(tokens)
  #tokens = nest_cbs(tokens)
  #tokens = split_blocks(tokens)
  #sp(tokens)
  parsed = parse_top(gram, 'top', tokens)
  assert parsed != None, 'Parse failed'
  #print parsed != None
  sp(srcish(parsed))
  #sp(parsed)

src = 'input.tmi'
pre = src + '.pre'
with open(src, 'r') as f:
  with open(pre, 'w') as pf:
    pf.write(hya(f.read()))

with open(pre, 'r') as f:
  presrc = f.read()
  #presrc = "let { " + presrc + " } in main"
parse(presrc)

# !! So cmemoize of subparse isn't faster, but it's working
# + case
# - inline let-in breaks non-inline let-in: we're popping the let at the dedent and then again when we are processing the 'in'.  We
#   have to pop at the dedent -- we pop until we no longer have a dedent.  When we reach an in, if it's a dedent in, then the let is popped,
#   but if it's an inline in, then it's not popped.
# + parentheses
# + parens to end a where
# + parens to end an of
# + parens to disambiguate nested inline wheres
# - throw in more parens for fun
# - dedented paren
# - Simpler method: stack is only indentations; when dedent, emit dedent and pop (loop that) -- might be able to parse linearly then
