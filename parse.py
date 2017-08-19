#!/usr/bin/python

import re

input_file = "input.tmi"

token_patterns = [
  { 'type': 'whitespace', 're': '(^\s+)(.*$)' },
  { 'type': 'string', 're': '("(\"|[^\"])*")(.*$)' },
  { 'type': 'identifier', 're': "(^[^\s]+)(.*$)" },
]

def tokenize_line(line_number, line):
  tokens = []
  column_number = 0
  while len(line) > 0:
    for token_pattern in token_patterns:
      m = re.match(token_pattern['re'], line)
      if m:
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

def tokenize(src):
  lines = src.split('\n')
  tokens_with_ws = [token for line_number, line in enumerate(lines) for token in tokenize_line(line_number, line)]
  if tokens_with_ws[0]['type'] == 'whitespace':
    tokens_with_ws = tokens_with_ws[1:]
  tokens = [tokens_with_ws[i] for i in xrange(0, len(tokens_with_ws)+1, 2)]
  return tokens

with open(input_file, 'r') as f:
  print tokenize(f.read())
