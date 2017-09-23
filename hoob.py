from collections import defaultdict
from lib import *
import sys

def flat1(os):
  return [o for oss in os for o in oss]

def linesr(os):
  return '\n'.join(os) + '\n'

def sh_grammar(gram):
  return linesr([nt + ' -> ' + ' | '.join([' '.join(opt) for opt in opts]) for nt, opts in gram])

_gram = [
  [ 'sentence', [ [ 'subject', 'predicate' ] ] ],
  [ 'subject', [ [ 'noun' ], [ 'adjective', 'noun' ] ] ],
  [ 'predicate', [ [ 'verb', 'noun' ], [ 'verb', 'a-n' ] ] ],
  [ 'a-n', [ [ 'adjective', 'noun' ] ] ],
]

gram = [
  [ 'sentence', [ [ 'subject', 'predicate' ] ] ],
  [ 'subject', [ [ 'noun' ], [ 'adjective', 'noun' ] ] ],
  [ 'predicate', [ [ 'verb', 'noun' ], [ 'verb', 'adjective', 'noun' ], [ 'verb', 'adjective', 'adjective', 'noun' ] ] ],
]

#@trace
def map_also(f, os):
  haha = map(f, os)
  #print 'uM', haha, zip(*haha), map(list, zip(*haha))
  return map(list, zip(*map(f, os)))
#print map_also(lambda x: (x*2, 'hey'+str(x)), [1, 2, 3])

new_sym_serial = 0
def new_sym():
  global new_sym_serial
  sym = 's' + str(new_sym_serial)
  new_sym_serial += 1
  return sym

#@trace
def reduce_long_opt(opt):
  if len(opt) > 2:
    nu = new_sym()
    return [[nu] + opt[2:], [[nu, [[opt[0], opt[1]]]]]]
  else:
    return [opt, []]

#@trace
def preprocess1(gram):
  newgram = []
  for rule in gram:
    print 'r', rule
    nt, opts = rule
    newopts, more_rules = map_also(reduce_long_opt, opts)
    newgram = newgram + [[nt, newopts]] + flat1(more_rules)
  return newgram

def fixpoint(f, x):
  new_x = f(x)
  return x if x == new_x else fixpoint(f, new_x)

def preprocess(gram):
  return fixpoint(preprocess1, gram)

ogram = gram
gram = preprocess(gram)
print sh_grammar(ogram)
print sh_grammar(gram)

#sys.exit(1)

tokens = [ 'adjective', 'noun', 'verb', 'adjective', 'adjective', 'noun' ]

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
    """
    a_ad_nts = ya_two(a, ad)
    dd_nts = ya_all(dd)
    type_two = [a for b in [ya_two(a, b) for a in a_ad_nts for b in dd_nts] for a in b]
    """
    a_ad_nts = ya_two(a, ad)
    type_two = flat1([ya_all([nt] + dd) for nt in a_ad_nts])
    return type_one + type_two

print ya_all(tokens)
