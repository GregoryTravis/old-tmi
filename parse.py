#!/usr/bin/python

import itertools
import re
import sys

def nest(tokens, pairs):
  lefts = [pair[0] for pair in pairs]
  rights = [pair[1] for pair in pairs]
  arr_stack = [[]]
  for token in tokens:
    if token in lefts:
      arr_stack.append([token])
    elif token in rights:
      arr_stack[-1].append(token)
      top = arr_stack.pop()
      arr_stack[-1].append(top)
    else:
      arr_stack[-1].append(token)
  assert len(arr_stack) == 1, arr_stack
  return arr_stack[0]

assert (nest(['a', 'aa', '(', 'b', 'bb', '(', 'd', ')', '(', 'e', ')', 'f', 'ff', ')', 'c', 'cc'], [('(', ')')]) ==
  ['a', 'aa', ['(', 'b', 'bb', ['(', 'd', ')'], ['(', 'e', ')'], 'f', 'ff', ')'], 'c', 'cc'])

assert (nest(['a', 'aa', '(', 'b', 'bb', '(', 'd', ')', 'let', 'e', 'in', 'f', 'ff', ')', 'c', 'cc'], [('(', ')'), ('let', 'in')]) ==
  ['a', 'aa', ['(', 'b', 'bb', ['(', 'd', ')'], ['let', 'e', 'in'], 'f', 'ff', ')'], 'c', 'cc'])
