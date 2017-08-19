#!/usr/bin/python

import re

input_file = "input.tmi"

token_patterns = [
  ['(^\s+)(.*$)', 'whitespace'],
  ["(^[^\s]+)(.*$)", 'identifier'],
  #['("(\"|[^\"])*")(.*$)', 'string'],
]

def tokenize_line(line_number, line):
  tokens = []
  column_number = 0
  while len(line) > 0:
    for token_pattern in token_patterns:
      m = re.match(token_pattern[0], line)
      if m:
        tokens.append([token_pattern[1], m.group(1), line_number, column_number])
        line = m.group(2)
        column_number += len(m.group(1))
        break
    else:
      raise 'Bad token', line
  return tokens

def tokenize(src):
  lines = src.split('\n')
  return [token for line_number, line in enumerate(lines) for token in tokenize_line(line_number, line)]

with open(input_file, 'r') as f:
  print tokenize(f.read())
