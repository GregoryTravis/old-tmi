from collections import defaultdict
from lib import *
from pprint import pprint
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
  #haha = map(f, os)
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
def all_reductions(os):
  def f(i):
    nu = new_sym()
    return [(os[0:i] + [nu] + os[i+2:]), [nu, [os[i:i+2]]]]
  return map_also(f, xrange(0, len(os)-1))

#@trace
def reduce_long_opt(opt):
  if len(opt) > 2:
    nu = new_sym()
    #print 'wel', nu, opt[0:2]
    opts, rules = all_reductions(opt)
    return [[nu], [[nu, opts]] + rules]
  else:
    return [opt, []]
#print reduce_long_opt(['a', 'b', 'c', 'd'])
#sys.exit(1)

#@trace
def preprocess1(gram):
  newgram = []
  for rule in gram:
    #print 'r', rule
    nt, opts = rule
    newopts, more_rules = map_also(reduce_long_opt, opts)
    newgram = newgram + [[nt, newopts]] + flat1(more_rules)
  #print '----'
  #print sh_grammar(newgram)
  return newgram

def fixpoint(f, x):
  new_x = f(x)
  return x if x == new_x else fixpoint(f, new_x)

def preprocess(gram):
  return fixpoint(preprocess1, gram)

#ogram = gram
#gram = preprocess(gram)
#print sh_grammar(ogram)
#print sh_grammar(gram)

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

#build_maps(gram)

#one_to_nts = dict(one_to_nts)
#two_to_nts = dict(two_to_nts)
#sp(one_to_nts, two_to_nts)

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
    #return ya_two(os[0], os[1])
    return flat1([ya_two(x, y) for x in ya_one(os[0]) for y in ya_one(os[1])])
  else:
    print 'type_one', os
    a, d = os[0], os[1:]
    a_nts = ya_one(a)
    d_nts = ya_all(d)
    #print os, a_nts, d_nts
    #type_one = [a for b in [ya_two(a, b) for a in a_nts for b in d_nts] for a in b]
    #vv = ya_one(a_nts)
    #vv2 = ya_one(d_nts)
    #print 'HEY', vv, vv2

    #type_one = flat1([ya_two(x, y) for x in a_nts for y in d_nts])
    #type_one = flat1([ya_two(x, y) for x in ya_one(a_nts) for y in ya_one(d_nts)])
    type_one = flat1([ya_two(x, y) for x in flat1(map(ya_one, a_nts)) for y in flat1(map(ya_one, d_nts))])

    print 'type_two', os
    ad = os[1]
    dd = os[2:]
    """
    a_ad_nts = ya_two(a, ad)
    dd_nts = ya_all(dd)
    type_two = [a for b in [ya_two(a, b) for a in a_ad_nts for b in dd_nts] for a in b]
    """
    #print 'TTWO', os, a, ad
    #haha = [[x, y, ya_two(x, y)] for x in ya_one(a) for y in ya_one(ad)]
    #print 'AAA', haha
    a_ad_nts = flat1([ya_two(x, y) for x in ya_one(a) for y in ya_one(ad)])
    hooo = flat1(map(ya_one, a_ad_nts))
    print 'tt', hooo
    #a_ad_nts = ya_two(a, ad)
    type_two = flat1([ya_all([nt] + dd) for nt in flat1(map(ya_one, a_ad_nts))])
    return flat1(map(ya_one, (type_one + type_two)))

#print ya_all(tokens)

"""
start = decls $;

decls = definition ';' decls | definition;

definition = defpat: {identifier}+ '=' body: exp;

exp = let | binopapp | app | identifier | integer;

let = 'let' '{' decls: decls '}' 'in' body: exp;

binopapp = left: identifier op: operator right: exp;

app = identifier {identifier}+;
"""

gram = [
  ['start', [['decls']]],
  ['decls', [['definition', 'semicolon', 'decls'], ['definition']]],
  ['definition', [['identifier_plus', 'equals', 'exp']]],
  ['identifier_plus', [['identifier_plus', 'identifier'], ['identifier']]],
  ['exp', [['let'], ['binopapp'], ['app'], ['identifier'], ['integer']]],
  ['let', [['let_keyword', 'lcb', 'decls', 'rcb', 'in_keyword', 'exp']]],
  ['binopapp', [['exp', 'operator', 'exp']]],
  ['app', [['identifier', 'identifier_plus']]],
]

#foo a b = let r b = b + b
           #in r

tokens = [ 'identifier', 'identifier', 'identifier', 'equals', 'let_keyword', 'lcb', 'identifier',
'identifier', 'equals', 'identifier', 'operator', 'identifier', 'rcb', 'in_keyword', 'identifier' ]

tokens = [ 'identifier', 'identifier', 'equals', 'identifier', 'identifier' ]

#tokens = [ 'let_keyword', 'lcb', 'identifier', 'identifier', 'equals', 'identifier', 'identifier', 'rcb', 'in_keyword', 'exp' ]
#tokens = [ 'identifier', 'identifier', 'equals', 'identifier', 'identifier', 'rcb', 'in_keyword', 'exp' ]
#tokens = [ 'identifier', 'identifier', 'equals', 'identifier', 'identifier', 'rcb' ]
#tokens = [ 'identifier', 'identifier', 'equals', 'identifier', 'identifier' ]

ogram = gram
#print sh_grammar(ogram)
gram = preprocess(gram)
print sh_grammar(ogram)
print sh_grammar(gram)

build_maps(gram)
#pprint(dict(one_to_nts))
#pprint(dict(two_to_nts))

print ya_all(tokens)

"""
gram = [
  [ 'a', [['b', 'c', 'd', 'e', 'f', 'g']] ]
]
print sh_grammar(gram)
gram = preprocess(gram)
print sh_grammar(gram)
build_maps(gram)
tokens = ['b', 'c', 'd', 'e', 'f', 'g']
print ya_all(tokens)
"""
