# -*- coding: UTF-8 -*-
from tabulate import tabulate

def d(**kwargs):
  return dict(**kwargs)

def ss(rel):
  fields = rel[0].keys() if len(rel) > 0 else ['']
  return tabulate([[rec[f] for f in fields] for rec in rel], fields, tablefmt='fancy_grid')
assert ss([d(a=1, b=2), d(a=10, b=20)]) == u'╒═════╤═════╕\n│   a │   b │\n╞═════╪═════╡\n│   1 │   2 │\n├─────┼─────┤\n│  10 │  20 │\n╘═════╧═════╛'

def s(*args):
  for o in args:
    print ss(o)
  return args[-1]

r = [
  d(a=1, b=2),
  d(a=10, b=20),
]
s(r, [])
