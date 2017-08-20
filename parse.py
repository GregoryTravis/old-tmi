#!/usr/bin/python

import itertools
import re
import sys

input_file = "input.tmi"

def concat(*arrs):
  return list(itertools.chain(*arrs))

#def spaces(n):
  #return ''.join([' ' for _ in xrange(n)])

def join_pred(os, pred):
  return os if len(os) < 2 else [os[0], pred(os[0], os[1])] + join_pred(os[1:], pred)

token_patterns = [
  { 'type': 'whitespace', 're': '(^\s+)(.*$)' },
  { 'type': 'string', 're': '("(\"|[^\"])*")(.*$)' },
  { 'type': 'identifier', 're': "(^[^\s]+)(.*$)" },
]

def semicolon(line_number):
  return { 'src': ';', 'first_in_line': False, 'line_number': line_number }

space = { 'src': ' ', 'first_in_line': False }
newline = { 'src': '\n', 'first_in_line': False }

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
      raise 'Bad token', line
  return tokens

def find_token(tokens, pred):
  for i, token in enumerate(tokens):
    if pred(token):
      return (tokens[0:i], tokens[i], tokens[i+1:])
  return None

def semi_for_lets(tokens):
  m = find_token(tokens, lambda token: token['src'] == 'in')
  if m:
    (before, in_token, after) = m
    assert len(after) > 0, 'dangling "in"'
    exp_start_column = after[0]['column_number']
    m2 = find_token(after, lambda token: token['first_in_line'] and token['column_number'] <= exp_start_column)
    if m2:
      (let_body, first, rest) = m2
      body_last_line = let_body[len(let_body)-1]['line_number']
      return concat(before, [in_token], let_body, [semicolon(body_last_line)], [first], rest)
    else:
      return concat(before, [in_token], after)
  else:
    return tokens

# In place
def add_first_in_line(tokens):
  current_line = None
  for token in tokens:
    if token['line_number'] != current_line:
      current_line = token['line_number']
      token['first_in_line'] = True
    else:
      token['first_in_line'] = False

def tokenize(src):
  lines = [line + '\n' for line in src.split('\n')]
  tokens_with_ws = [token for line_number, line in enumerate(lines) for token in tokenize_line(line_number, line)]
  tokens = [token for token in tokens_with_ws if token['type'] != 'whitespace']
  add_first_in_line(tokens)
  return tokens

def concat_tokens(tokens):
  return ''.join([token['src'] for token in tokens])

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

with open(input_file, 'r') as f:
  tokens = tokenize(f.read())

tokens = semi_for_lets(tokens)
print tokens_to_src(tokens)

#-- still lexing wrong, maybe \n isn't being parsed as ws?
