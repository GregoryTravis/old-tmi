from collections import defaultdict
from lib import *

gram = [
  [ 'sentence', [ [ 'subject', 'predicate' ] ] ],
  [ 'subject', [ [ 'noun' ], [ 'adjective', 'noun' ] ] ],
  [ 'predicate', [ [ 'verb', 'noun' ], [ 'verb', 'a-n' ] ] ],
  [ 'a-n', [ [ 'adjective', 'noun' ] ] ],
]

tokens = [ 'adjective', 'noun', 'verb', 'adjective', 'noun' ]

two_to_nts = defaultdict(list)
one_to_nts = defaultdict(list)

def build_maps(gram):
  for rule in gram:
    nt, opts = rule[0], rule[1]
    for opt in opts:
      if len(opt) == 1:
        one_to_nts[(opt[0],)].append(nt)
      elif len(opt) == 2:
        two_to_nts[(opt[0],opt[1])].append(nt)
      else:
        assert False, opt

build_maps(gram)

#one_to_nts = dict(one_to_nts)
#two_to_nts = dict(two_to_nts)
sp(one_to_nts, two_to_nts)

@trace
def ya_one(token):
  return [token] + one_to_nts[(token,)]
#print ya_one('noun')

@trace
def ya_two(a, b):
  return two_to_nts[(a, b)]
#print ya_two('adjective', 'noun')

@trace
def ya_all(os):
  assert len(os) > 0
  if len(os) == 1:
    return ya_one(os[0])
  elif len(os) == 2:
    return ya_two(os[0], os[1])
  else:
    a, d = os[0], os[1:]
    a_nts = ya_one(a)
    d_nts = ya_all(d)
    #print os, a_nts, d_nts
    type_one = [a for b in [ya_two(a, b) for a in a_nts for b in d_nts] for a in b]
    ad = os[1]
    dd = os[2:]
    a_ad_nts = ya_two(a, ad)
    dd_nts = ya_all(dd)
    type_two = [a for b in [ya_two(a, b) for a in a_ad_nts for b in dd_nts] for a in b]
    return type_one + type_two

print ya_all(tokens)
